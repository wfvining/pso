%%% @author Will Vining <wfvining@gmail.com>
%%% @copyright (C) 2021, Will Vining
%%% @doc
%%%
%%% Server that manages the state of a single particle.
%%%
%%% The server maintains an internal clock representing the number of
%%% iterations it has evaluated. In addition, it maintains a copy of
%%% the position and value of all its neighboring particles. The list
%%% is updated by casts from the neighboring particles sent whenever
%%% they have completed an iteration.
%%%
%%% @end
%%% Created : 15 May 2021 by Will Vining <wfvining@gmail.com>

-module(particle_server).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3]).

-export([eval/2, state/1, set_neighbors/2,
         start_link/4, report_state/2]).

-record(state, {particle :: particle:particle(),
                neighbors = [] :: [pid()],
                neighbor_clock = #{} :: #{ pid() => non_neg_integer() },
                neighbor_state = #{} :: #{ pid() => particle:state()},
                iteration = 1 :: pos_integer(),
                stop_iteration = 1 :: pos_integer(),
                evaluator = idle :: idle | pid()}).

-type state() :: #state{}.

%% @doc
%% Start a particle server for the particle with the given `Position',
%% `Velocity', and `ObjectiveFun'. To specify the minimum and maximum
%% position pass ``[{bounds, {MinPosition, MaxPosition}}]'' in
%% `Options'.
%% @end
-spec start_link(Position :: particle:position(),
                 Velocity :: particle:velocity(),
                 ObjectiveFun :: particle:objective(),
                 Options :: proplists:proplist()) -> {ok, pid()}.
start_link(Position, Velocity, ObjectiveFun, Options) ->
    case proplists:get_value(bounds, Options, nil) of
        {MinBound, MaxBound} ->
            gen_server:start_link(
              ?MODULE,
              [Position, Velocity, ObjectiveFun, MinBound, MaxBound],
              []);
        nil ->
            gen_server:start_link(
              ?MODULE,
              [Position, Velocity, ObjectiveFun],
              [])
    end.

-spec eval(ParticleServer :: pid(), Iterations :: pos_integer()) -> ok.
eval(ParticleServer, Iterations) ->
    gen_server:cast(ParticleServer, {eval, Iterations}).

-spec state(ParticleServer :: pid()) -> particle:state().
state(ParticleServer) ->
    gen_server:call(ParticleServer, get_state).

-spec set_neighbors(ParticleServer :: pid(), Neighbors :: [pid()]) -> ok.
set_neighbors(ParticleServer, Neighbors) ->
    gen_server:cast(ParticleServer, {set_neighbors, Neighbors}).

-spec report_state(ParticleServer :: pid(), State :: state()) -> ok.
report_state(ParticleServer,
             #state{ particle = Particle, iteration = Iteration }) ->
    gen_server:cast(
      ParticleServer,
      {report_state, self(), particle:state(Particle), Iteration}).

init(ParticleArgs) ->
    {ok,
     #state{particle = apply(particle, new, ParticleArgs),
            iteration = 1}}.

handle_call({eval, _}, _From, State = #state{iteration = Iteration,
                                             stop_iteration = StopIteration})
  when Iteration < StopIteration ->
    {reply, {already_iterating, StopIteration}, State};
handle_call({eval, Iterations}, From, State = #state{ iteration = Iteration}) ->
    % Set the stop_iteration, then do a try_iteration.
    % Since I want to abstract the try_iteration pattern, I'm going to use
    % gen_server:reply/2, and then return a `{noreply, ...}' tuple.
    StopIteration = Iteration + Iterations,
    gen_server:reply(From, {ok, StopIteration}),
    NewState = State#state{stop_iteration = StopIteration},
    case try_iteration(NewState) of
        not_ready ->
            {noreply, NewState};
        {ok, Evaluator} ->
            {noreply, NewState#state{ evaluator = Evaluator }}
    end;
handle_call(get_state, _From, State = #state{particle = Particle}) ->
    {reply, particle:state(Particle), State}.

handle_cast({set_neighbors, Neighbors},
            State = #state{ neighbor_clock = Clocks,
                            neighbor_state = NeighborState }) ->
    %% Only add clocks for previously unknown particles, the state will
    %% be left unspecified. By leaving the state unspecified an error
    %% will be raised if we try to get the state before a
    NewClocks = maps:from_list([{N, 0} || N <- Neighbors]),
    report_state(State),
    NewState = State#state{
                %% Discard any information about neighbors not in `Neighbors'
                neighbor_clock = maps:with(Neighbors, maps:merge(Clocks, NewClocks)),
                neighbor_state = maps:with(Neighbors, NeighborState)},
    case try_iteration(NewState) of
        not_ready ->
            {noreply, NewState};
        {ok, Evaluator} ->
            {noreply, NewState#state{ evaluator = Evaluator }}
    end;
handle_cast({report_state, Neighbor, NeighborState, Iteration},
            State = #state{ neighbor_clock = Clocks,
                            neighbor_state = Neighbors}) ->
    NewClocks = maps:update_with(
                  Neighbor,
                  fun (I) when I < Iteration -> Iteration;
                      (I) when I >= Iteration -> I
                  end,
                  Iteration,
                  Clocks),
    Updated = maps:get(Neighbor, NewClocks),
    NewNeighbors = maps:update_with(
                     Neighbor,
                     fun(_) when Updated =:= Iteration -> NeighborState;
                        (S) -> S
                     end,
                     NeighborState,
                     Neighbors),
    NewState = State#state{ neighbor_clock = NewClocks,
                            neighbor_state = NewNeighbors },
    case try_iteration(NewState) of
        not_ready ->
            {noreply, NewState};
        {ok, Evaluator} ->
            {noreply, NewState#state{ evaluator = Evaluator }}
    end.

%% Evaluator finished evaluating the next iteration. Increment the
%% current iteration and replace the particle state with the new
%% particle state.
handle_info({evaluation_result, Evaluator, NewParticle},
            State = #state{ evaluator = Evaluator, iteration = Iteration }) ->
    NewState = State#state{ particle = NewParticle,
                            evaluator = idle,
                            iteration = Iteration + 1 },
    report_state(NewState),
    case try_iteration(NewState) of
        not_ready ->
            {noreply, NewState};
        {ok, NewEvaluator} ->
            {noreply, NewState#state{ evaluator = NewEvaluator}}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Report the particle's state to its neighbors.
report_state(State = #state{ neighbors = Neighbors }) ->
    lists:foreach(fun(N) -> report_state(N, State) end, Neighbors).

%% If all neighbors are accounted for at the current time, then
%% evaluate an iteration of the PSO algorithm.
-spec try_iteration(state()) -> not_ready | {ok, pid()}.
try_iteration(#state{ evaluator = Evaluator }) when is_pid(Evaluator) ->
    not_ready;
try_iteration(#state{ neighbors = [] }) ->
    not_ready;
try_iteration(#state{ neighbors = Neighbors,
                      neighbor_clock = Clocks,
                      neighbor_state = NeighborState,
                      iteration = CurrentIteration,
                      stop_iteration = StopIteration,
                      particle = Particle,
                      evaluator = idle })
  when StopIteration > CurrentIteration ->
    Ready = lists:all(
              fun(Neighbor) ->
                      maps:get(Neighbor, Clocks) >= CurrentIteration
              end, Neighbors),
    if Ready ->
            Server = self(),
            {ok,
             spawn_link(
               fun() ->
                       NewParticle =
                           particle:step(
                             Particle,
                             maps:values(maps:with(Neighbors, NeighborState))),
                       Server ! {evaluation_result, self(), NewParticle}
               end)};
       not(Ready) ->
            not_ready
    end.
