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

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3,
         terminate/2]).

-export([eval/2, state/1, state/2, set_neighbors/2, start_link/4, stop/1]).

-record(state, {particle :: particle:particle(),
                neighbors = [] :: [pid()],
                neighbor_clock = #{} :: #{ pid() => non_neg_integer() },
                neighbor_state = #{} :: #{ pid() => particle:state()},
                iteration = 1 :: pos_integer(),
                stop_iteration = 1 :: pos_integer(),
                evaluator = idle :: idle | pid(),
                pending_requests = #{} :: #{pos_integer() := [{pid(), term()}]}}).

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

%% @doc
%% Evaluate `Iterations' interations of the PSO algorithm. The
%% iteration number of the final iteration that will be evaluated is
%% returned. (For example, if the server has already evaluated 10
%% iterations, and we request a further 12 iterations, ``{ok, 22}'' is
%% returned.)
%% @end
-spec eval(ParticleServer :: pid(), Iterations :: pos_integer())
          -> {ok, FinalIteration :: pos_integer()}
              | {already_iterating, FinalIteration :: pos_integer()}.
eval(ParticleServer, Iterations) ->
    gen_server:call(ParticleServer, {eval, Iterations}).

%% @doc Get the current state of the particle managed by this server.
-spec state(ParticleServer :: pid()) -> particle:state().
state(ParticleServer) ->
    gen_server:call(ParticleServer, get_state).

%% @doc
%% Get the state of the particle after `Iteration' iterations have
%% been completed. The returned state may be from any iteration
%% *after* `Iteration', but not before. If `Iteration' has not been
%% reached this call will block until it has been completed.
%% @end
-spec state(ParticleServer :: pid(), Iteration :: pos_integer())
           -> particle:state().
state(ParticleServer, Iteration) ->
    gen_server:call(ParticleServer, {get_state, Iteration}, infinity).

%% @doc Stop the server.
-spec stop(ParticleServer :: pid()) -> ok.
stop(ParticleServer) ->
    gen_server:stop(ParticleServer).

%% @doc Set the particle server's neighbors.
-spec set_neighbors(ParticleServer :: pid(), Neighbors :: [pid()]) -> ok.
set_neighbors(ParticleServer, Neighbors) ->
    gen_server:cast(ParticleServer, {set_neighbors, Neighbors}).

init(ParticleArgs) ->
    {ok,
     #state{particle = apply(particle, new, ParticleArgs),
            iteration = 1}}.

handle_call({eval, _}, _From, State = #state{iteration = Iteration,
                                             stop_iteration = StopIteration})
  when Iteration < StopIteration ->
    {reply, {already_iterating, StopIteration}, State};
handle_call({eval, Iterations}, _From,
            State = #state{ iteration = Iteration}) ->
    StopIteration = Iteration + Iterations,
    {reply, {ok, StopIteration},
     try_iteration(State#state{stop_iteration = StopIteration})};
handle_call({get_state, AtIteration}, _From,
            State = #state{ iteration = Iteration,
                            particle = Particle })
  when Iteration >= AtIteration ->
    %% Already at or past the requested iteration, return immediately.
    {reply, particle:state(Particle), State};
handle_call({get_state, AtIteration}, From,
            State = #state{ pending_requests = PendingRequests}) ->
    {noreply,
     State#state{
       pending_requests = add_pending_request(AtIteration, From, PendingRequests)}};
handle_call(get_state, _From, State = #state{particle = Particle}) ->
    {reply, particle:state(Particle), State}.

handle_cast({set_neighbors, Neighbors},
            State = #state{ neighbor_clock = Clocks,
                            neighbor_state = NeighborState }) ->
    %% Only add clocks for previously unknown particles, the state will
    %% be left unspecified. By leaving the state unspecified an error
    %% will be raised if we try to get the state before a
    NewClocks = maps:from_list([{N, 0} || N <- Neighbors]),
    NewState = State#state{
                 neighbors = Neighbors,
                 %% Discard any information about neighbors not in `Neighbors'
                 neighbor_clock = maps:with(Neighbors, maps:merge(Clocks, NewClocks)),
                 neighbor_state = maps:with(Neighbors, NeighborState)},
    report_state_to_neighbors(NewState),
    {noreply, try_iteration(NewState)};
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
    {noreply, try_iteration(NewState)}.

%% Evaluator finished evaluating the next iteration. Increment the
%% current iteration and replace the particle state with the new
%% particle state.
handle_info({evaluation_result, Evaluator, NewParticle},
            State = #state{ evaluator = Evaluator,
                            iteration = Iteration,
                            pending_requests = PendingRequests }) ->
    NewState = State#state{ particle = NewParticle,
                            evaluator = idle,
                            iteration = Iteration + 1 },
    report_state_to_neighbors(NewState),
    handle_pending_requests(
      Iteration + 1, particle:state(NewParticle), PendingRequests),
    {noreply, try_iteration(NewState)}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{evaluator = idle}) -> ok;
terminate(Reason, #state{evaluator = Evaluator}) ->
    %% If there is an evaluator running, unlink from it and kill it
    %% with `Reason'.
    erlang:unlink(Evaluator),
    exit(Evaluator, Reason).

%% @doc Report the `Iteration' and state of `Particle' to `ParticleServer'.
-spec report_state(ParticleServer :: pid(),
                   Particle :: particle:particle(),
                   Iteration :: pos_integer()) -> ok.
report_state(ParticleServer, Particle, Iteration) ->
    gen_server:cast(
      ParticleServer,
      {report_state, self(), particle:state(Particle), Iteration}).

%% @doc Report the particle's state to all of its neighbors.
-spec report_state_to_neighbors(State :: state()) -> ok.
report_state_to_neighbors(#state{ neighbors = Neighbors,
                                  particle = Particle,
                                  iteration = Iteration }) ->
    lists:foreach(fun(N) -> report_state(N, Particle, Iteration) end, Neighbors).

%% @doc
%% If the state of all neighbors at the current current iteration is
%% known, then spawn a new process to evaluate the next iteration of
%% the PSO algorithm. If an evaluator is already running, or no
%% neighbors have been assigned then no iteration is performed.
%% @end
-spec try_iteration(state()) -> state().
try_iteration(State = #state{ evaluator = Evaluator }) when is_pid(Evaluator) ->
    State;
try_iteration(State = #state{ neighbors = [] }) ->
    State;
try_iteration(State = #state{ neighbors = Neighbors,
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
            State#state{
              evaluator =
                  spawn_link(
                    fun() ->
                            NewParticle =
                                particle:step(
                                  Particle,
                                  maps:values(maps:with(Neighbors, NeighborState))),
                            Server ! {evaluation_result, self(), NewParticle}
                    end)};
       not(Ready) ->
            State
    end;
try_iteration(State = #state{ iteration = CurrentIteration,
                              stop_iteration = StopIteration})
  when StopIteration =:= CurrentIteration ->
    State.


-spec add_pending_request(At :: pos_integer(),
                          Requestor :: {pid(), Tag},
                          PendingRequests)
                         -> PendingRequests
              when PendingRequests :: #{ pos_integer() := [{pid(), Tag}]}.
add_pending_request(At, Requestor, PendingRequests) ->
    maps:update_with(
      At,
      fun (Requestors) -> [Requestor|Requestors] end,
      [Requestor],
      PendingRequests).

-spec handle_pending_requests(At :: pos_integer(),
                              ParticleState :: particle:state(),
                              PendingRequests) -> PendingRequests
              when PendingRequests :: #{pos_integer() := [{pid(), term()}]}.
handle_pending_requests(At, ParticleState, PendingRequests) ->
    case maps:take(At, PendingRequests) of
        error ->
            PendingRequests;
        {Requestors, NewPendingRequests} ->
            lists:foreach(
              fun (Caller) -> gen_server:reply(Caller, ParticleState) end,
              Requestors),
            NewPendingRequests
    end.
