%%% @author Will Vining <wfvining@gmail.com>
%%% @copyright (C) 2021, Will Vining
%%% @doc
%%% A particle has a poistion, a velocity, and a set of neighbors.
%%%
%%% Upon initialization a particle enters into a loop:
%%% <ul>
%%% <li> the value at its current position is computed </li>
%%% <li> the particle waits to be instructed to continue </li>
%%% <li> the particle sends its position and value to all its neighbors </li>
%%% <li> it awaits the current value and position from all other neighbors </li>
%%% <li> it updates its position and velocity according to the information </li>
%%%   received from the neighbors.
%%% </ul>
%%% @end
%%% Created : 21 Feb 2021 by Will Vining <wfvining@gmail.com>

-module(particle).

-behavior(gen_statem).

%% api functions
-export([start_link/3, assign_neighbors/2, step/1]).
%% gen_statem callbacks
-export([init/1, callback_mode/0]).
%% state callbacks
-export([initialize/3, accelerate/3, eval/3]).

-type velocity() :: term().
-type position() :: term().
-type value() :: term().

-record(state, {position :: position(),
                pbest :: position(),
                vbest :: any(),
                velocity :: velocity(),
                neighbors = [] :: list(pid()),
                received = [] :: list({position(), term()}),
                refs = [] :: list(reference()),
                value :: value(),
                module :: module()}).

-type particle_state() :: #state{}.
-type neighbor_state() :: {position(), value()}.

%% Evaluate the objective function.
-callback eval(Position :: any()) ->
    {ok, Value :: any()} | {error, Reason :: any()}.
%% Move the particle.
-callback move(Position :: any(), Velocity :: velocity()) ->
    {ok, NewPosition :: any()}.

%% Acceleration requires the following three operations

%% Return `PositionA' minus `PositionB' (e.g. the displacement between
%% the positions).
-callback displacement(PositionA :: position(), PositionB :: position()) ->
    position().

%% Multiply `Velocity' by `Scale'
-callback scale_velocity(Scale :: float(), Velocity :: velocity()) ->
    velocity().

%% Add two velocities.
-callback add_velocity(VelocityA :: velocity(), VelocityB :: velocity()) ->
    velocity().

%% Return true if `ValueA' is less than or equal to `ValueB'.
-callback compare(ValueA :: any(), ValueB :: any()) -> boolean().

%% @doc
%% Start a particle and link to its process. The particle is initially
%% at `Position' and moving at `Velocity'. `Module' is the callback
%% module implementing the `particle' behavior.
%% @end
-spec start_link(position(), velocity(), module()) -> {ok, pid()}.
start_link(Position, Velocity, Module) ->
    gen_statem:start_link(?MODULE, {Position, Velocity, Module}, []).

%% @doc Assign `Neighbors' as the neighbors of `Particle'.
-spec assign_neighbors(Particle :: pid(), Neighbors :: list(pid())) -> ok.
assign_neighbors(Particle, Neighbors) ->
    gen_statem:cast(Particle, {neighbors, Neighbors}).

%% @doc Step the particle through a single iteration of the PSO algorithm.
-spec step(Particle :: pid()) -> ok.
step(Particle) ->
    gen_statem:cast(Particle, continue).

callback_mode() ->
    [state_functions, state_enter].

init({Position, Velocity, Module}) ->
    {ok, initialize, #state{ position = Position,
                             velocity = Velocity,
                             module = Module}}.

%% Apply the PSO acceleration algorithm to the particle state.
-spec accelerate_particle(Data :: particle_state(), list(neighbor_state()))
                         -> velocity().
accelerate_particle(#state{pbest = PBest,
                           position = Position,
                           module = Module,
                           velocity = Velocity},
                    Neighbors) ->
    {NeighborBest, _} = lists:last(
                          lists:sort(
                            fun({_, ValueA}, {_, ValueB}) ->
                                    Module:compare(ValueA, ValueB)
                            end,
                            Neighbors)),
    BestDisplacement = Module:displacement(PBest, Position),
    NeighborhoodDisplacement = Module:displacement(NeighborBest, Position),
    Module:add_velocity(
      Module:add_velocity(
        Velocity,
        Module:scale_velocity(2*rand:uniform(), BestDisplacement)),
      Module:scale_velocity(2*rand:uniform(), NeighborhoodDisplacement)).

%% @doc
%% Particles begin executing in this state. Here they wait for a list
%% of neighboring particle PIDs to be provided, then for a singal that
%% they should begin execution.
%% @end
initialize(enter, _, Data) ->
    {keep_state, Data};
initialize(cast, start, Data) ->
    {next_state, eval, Data};
initialize(cast, {neighbors, Neighbors}, Data) ->
    Refs = [monitor(process, Neighbor) || Neighbor <- Neighbors],
    {keep_state, Data#state{ neighbors = Neighbors, refs = Refs }}.

%% @doc
%% Evaluate the current position then wait for a signal to continue
%% before accelerating the particle.
%% @end
eval(enter, initialize, Data = #state{ position = Position,
                                       module = Module }) ->
    Value = Module:eval(Position),
    {keep_state, Data#state{value = Value, pbest = Position}};
eval(enter, _OldState, Data = #state{ position = Position,
                                      vbest = VBest,
                                      module = Module }) ->
    Value = Module:eval(Position),
    case Module:compare(VBest, Value) of
        true ->
            {keep_state, Data#state{ value = Value,
                                     pbest = Position,
                                     vbest = Value }};
        false ->
            {keep_state, Data#state{ value = Value }}
    end;
eval(cast, continue, Data) ->
    {next_state, accelerate, Data};
eval(cast, {value, _, _}, Data) ->
    % Postpone values received from neighboring particles until the
    % `accelerate' state.
    {keep_state, Data, {postpone, true}}.

%% @doc
%% Upon entry into the accelerate state the particle reports its
%% current position and value to its neighboring particles. The
%% particle then waits until all its neighbors have reported their
%% position and value at which point its position is updated and it
%% is accelerated.
%% @end
accelerate(enter, _OldState, Data = #state{position = Position,
                                           value = Value}) ->
    lists:foreach(
      fun(Neighbor) ->
              gen_statem:cast(Neighbor, {value, Position, Value})
      end,
      Data#state.neighbors);
accelerate(cast, {value, Position, Value},
           Data = #state{ received = Received, neighbors = Neighbors }) ->
    % If all neighbors are accounted for then accelerate the particle
    % and transition to `eval' state. Since edges are undirected (in
    % this initial model) we can check this by comparing the length of
    % the received list with the length of the neighbors list.
    case [{Position, Value}|Received] of
        NewReceived when length(NewReceived) =:= length(Neighbors) ->
            {next_state, eval,
             Data#state{ received = [],
                         % XXX this returns a velocity, not a
                         % position. The updated velocity should be
                         % applied to the current position prior to
                         % the evaluation of the new position.
                         position = accelerate_particle(Data, NewReceived)}};
        NewReceived ->
            {keep_state, Data#state{received = NewReceived}}
    end.
