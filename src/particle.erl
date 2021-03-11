%%% @author Will Vining <wfvining@gmail.com>
%%% @copyright (C) 2021, Will Vining
%%% @doc
%%% A particle has a poistion, a velocity, and a set of neighbors.
%%%
%%% Upon initialization a particle enters into a loop:
%%% - the value at its current position is computed
%%% - the particle sends its position and value to all its neighbors
%%% - it awaits the current value and position from all other neighbors
%%% - it updates its position and velocity according to the information
%%%   received from the neighbors.
%%% @end
%%% Created : 21 Feb 2021 by Will Vining <wfvining@gmail.com>

-module(particle).

-behavior(gen_statem).

-export([start_link/3, assign_neighbors/2]).
-export([init/1, callback_mode/0]).
-export([initialize/3, accelerate/3, eval/3]).

-type velocity() :: term().
-type position() :: term().

-record(state, {position :: position(),
                pbest :: position(),
                vbest :: any(),
                velocity :: velocity(),
                neighbors = [] :: list(pid()),
                received = [] :: list({position(), term()}),
                refs :: list(reference()),
                value :: any(),
                module :: module()}).

%% Evaluate the objective function.
-callback eval(Position :: any()) ->
    {ok, Value :: any()} | {error, Reason :: any()}.
%% Move the particle.
-callback move(Position :: any(), Velocity :: velocity()) ->
    {ok, NewPosition :: any()}.

%% Acceleration requires the following three operations

%% @doc
%% Return `PositionA' minus `PositionB' (e.g. the displacement between
%% the positions).
%% @end
-callback displacement(PositionA :: position(), PositionB :: position()) ->
    position().

%% @doc Multiply `Velocity' by `Scale'
-callback scale_velocity(Scale :: float(), Velocity :: velocity()) ->
    velocity().

%% @doc Add two velocities.
-callback add_velocity(VelocityA :: velocity(), VelocityB :: velocity()) ->
    velocity().

%% @doc Return true if `ValueA' is less than or equal to `ValueB'.
-callback compare(ValueA :: any(), ValueB :: any()) -> boolean().

start_link(Position, Velocity, Module) ->
    gen_statem:start_link(?MODULE, {Position, Velocity, Module}, []).

%% @doc Assign `Neighbors' as the neighbors of `Particle'.
-spec assign_neighbors(Particle :: pid(), Neighbors :: list(pid())) -> ok.
assign_neighbors(Particle, Neighbors) ->
    gen_statem:cast(Particle, {neighbors, Neighbors}).

callback_mode() ->
    [state_functions, state_enter].

init({Position, Velocity, Module}) ->
    {ok, initialize, #state{ position = Position,
                             velocity = Velocity,
                             module = Module}}.

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

initialize(enter, _, Data) ->
    {keep_state, Data};
initialize(cast, start, Data) ->
    {next_state, eval, Data};
initialize(cast, {neighbors, Neighbors}, Data) ->
    Refs = [monitor(process, Neighbor) || Neighbor <- Neighbors],
    {keep_state, Data#state{ neighbors = Neighbors, refs = Refs }}.

eval(enter, initialize, Data = #state{ position = Position,
                                       module = Module }) ->
    Value = Module:eval(Position),
    {next_state, accelerate, Data#state{value = Value, pbest = Position}};
eval(enter, _OldState, Data = #state{ position = Position,
                                      vbest = VBest,
                                      module = Module }) ->
    Value = Module:eval(Position),
    case Module:compare(VBest, Value) of
        true ->
            {next_state, accelerate, Data#state{ value = Value,
                                                 pbest = Position,
                                                 vbest = Value }};
        false ->
            {next_state, accelerate, Data#state{ value = Value }}
    end.

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
                         position = accelerate_particle(Data, NewReceived)}};
        NewReceived ->
            {keep_state, Data#state{received = NewReceived}}
    end.
