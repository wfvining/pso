%%% @author Will Vining <wfvining@gmail.com>
%%% @copyright (C) 2021, Will Vining
%%% @doc
%%% Server that manages the state of a single particle.
%%% @end
%%% Created : 15 May 2021 by Will Vining <wfvining@gmail.com>

-module(particle_server).

-behavior(gen_server).

-export([start_link/4, set_neighbors/2, report_state/2, step/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         code_change/3]).

-type neighbor_state() :: {particle:position(), particle:value()}.

-record(state, {particle :: particle:particle(),
                neighbors = [] :: [pid()],
                neighbor_state = #{} :: #{ pid() => neighbor_state() }}).

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

%% @doc Evaluate one step of the PSO algorithm.
-spec step(ParticleServer :: pid()) -> {particle:position(), particle:value()}.
step(ParticleServer) ->
    gen_server:call(ParticleServer, step).

%% @doc Tell `ParticleServer' who its neighbors are.
-spec set_neighbors(ParticleServer :: pid(), Neighbors :: [pid()]) -> ok.
set_neighbors(ParticleServer, Neighbors) ->
    gen_server:cast(ParticleServer, {set_neighbors, Neighbors}).

%% @doc Send `State' to `Neighbor' for use in its next update.
-spec report_state(Neighbor :: pid(),
                   State :: neighbor_state()) -> ok.
report_state(Neighbor, State) ->
    gen_server:cast(Neighbor, {state, self(), State}).

init(ParticleArgs) ->
    {ok, #state{particle = apply(particle, new, ParticleArgs)}}.

handle_cast({set_neighbors, Neighbors}, State) ->
    {noreply, State#state{neighbors = Neighbors}};
handle_cast({state, Neighbor, NeighborState},
            State = #state{ neighbor_state = Neighborhood }) ->
    {noreply,
     State#state{
       neighbor_state = Neighborhood#{Neighbor => NeighborState}}}.

handle_call(step, _From, State = #state{particle = Particle,
                                        neighbor_state = Neighbors}) ->
    NewParticle = particle:step(Particle, maps:values(Neighbors)),
    {reply,
     particle:state(NewParticle),
     State#state{particle = NewParticle,
                 neighbor_state = #{}}, % clear the neighborhood state
     {continue, report_state}};
handle_call(get_state, _From, State = #state{particle = Particle}) ->
    {reply, particle:state(Particle), State}.

handle_continue(report_state, State = #state{particle = Particle,
                                             neighbors = Neighbors}) ->
    lists:foreach(
      fun(Neighbor) ->
              report_state(Neighbor, particle:state(Particle))
      end,
      Neighbors),
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
