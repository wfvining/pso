%%% @author Will Vining <wfvining@gmail.com>
%%% @copyright (C) 2021, Will Vining
%%% @doc
%%% A particle for a minimizing particle swarm optimization problem.
%%% @end
%%% Created : 25 Apr 2021 by Will Vining <wfvining@gmail.com>

-module(particle).

-export([new/3, step/2, position/1, value/1, state/1]).

-export_type([particle/0, position/0]).

-type vector() :: [float()].
-type position() :: vector().
-type velocity() :: vector().
-type value() :: term().
-type objective_fun() :: fun((position()) -> value()).

-record(particle, {position = [0.0] :: position(),
                   velocity = [0.0] :: velocity(),
                   value :: value(),
                   objective :: objective_fun(),
                   best_position = [0.0] :: position(),
                   best_value = 0.0 :: value()}).

-opaque particle() :: #particle{}.

-define(PHI, 1.49618).
-define(OMEGA, 0.7298).

%% @doc
%% Initialize a new particle at `Position'. The particle's initial
%% velocity is `Velocity' and its objective function is computed by
%% `ObjectiveFun'. Immediately upon creation (before returning) the
%% particle's initial value is calculated by calling
%% ``ObjectiveFun(Position)''.
%% @end
-spec new(Position :: position(),
          Velocity :: velocity(),
          ObjectiveFun :: fun((position()) -> value())) -> particle().
new(Position, Velocity, ObjectiveFun) ->
    Value = ObjectiveFun(Position),
    #particle{position = Position,
              velocity = Velocity,
              value = Value,
              objective = ObjectiveFun,
              best_position = Position,
              best_value = Value}.

%% @doc
%% Apply the original PSO acceleration algorithm to `Velocity'.
%% @end
-spec accelerate(Velocity :: velocity(),
                 Position :: position(),
                 PersonalBest :: position(),
                 Neighbors :: [{position(), value()}]) -> velocity().
accelerate(Velocity, CurrentPosition, BestPosition, Neighbors) ->
    [{GlobalBest, _}|_] = lists:keysort(2, Neighbors),
    SelfAcceleration = vector:multiply(?PHI * rand:uniform_real(),
                         vector:subtract(BestPosition, CurrentPosition)),
    GlobalAcceleration = vector:multiply(?PHI * rand:uniform_real(),
                           vector:subtract(BestPosition, GlobalBest)),
    vector:add(
      vector:multiply(?OMEGA, Velocity),
      vector:add(SelfAcceleration, GlobalAcceleration)).

%% @doc Return the current position of `Particle'.
-spec position(Particle :: particle()) -> position().
position(#particle{position = Position}) -> Position.

%% @doc
%% Return the particle's value. If the value is undefined an error is
%% thrown.
%% @end
-spec value(Particle :: particle()) -> value().
value(#particle{value = Value}) -> Value.

%% @doc Return the position and value of `Particle'.
-spec state(Particle :: particle()) -> {position(), value()}.
state(Particle) ->
    {position(Particle), value(Particle)}.

%% @doc Evaluate the objective function at the particle's current position.
-spec eval(Particle :: particle(), Position :: position()) -> particle().
eval(Particle = #particle{best_value = BestValue,
                          objective = ObjectiveFun},
     Position) ->
    NewValue = ObjectiveFun(Position),
    if
        NewValue < BestValue ->
            Particle#particle{best_value = NewValue,
                              best_position = Position,
                              value = NewValue,
                              position = Position};
        NewValue >= BestValue ->
            Particle#particle{value = NewValue, position = Position}
    end.

%% @doc
%% Evaluate a single iteration of the PSO algorithm. The velocity of
%% `Particle' is updated under the influence of `Neighbors' and the
%% new velocity is applied to update the particle's position.
%%
%% `Neighbors' should contain the best solution found so fajr by each
%% of the particles that influences this particle.
%% @end
-spec step(Particle :: particle(), Neighbors :: [{position(), value()}]) -> particle().
step(Particle, Neighbors) ->
    NewVelocity =  accelerate(Particle#particle.velocity,
                              Particle#particle.position,
                              Particle#particle.best_position,
                              Neighbors),
    NewPosition = vector:add(Particle#particle.position, NewVelocity),
    eval(Particle#particle{velocity = NewVelocity}, NewPosition).
