-module(test_particle).

-export([move/2, eval/1, displacement/2, scale_velocity/2,
         add_velocity/2, compare/2]).

-include_lib("eunit/include/eunit.hrl").

move(X, V) ->
    {ok, X + V}.

eval(X) ->
    {ok, X*X}.

displacement(X, Y) ->
    X - Y.

scale_velocity(S, V) ->
    S * V.

add_velocity(V, Q) ->
    V + Q.

compare(X, Y) ->
    X =< Y.

start_particle_right() ->
    {ok, Pid} = particle:start_link(0.0, 3.0, ?MODULE),
    Pid.

start_particle_left() ->
    {ok, Pid} = particle:start_link(0.0, -3.0, ?MODULE),
    Pid.

run_both_particles_separately() ->
    Left = start_particle_left(),
    Right = start_particle_right(),
    particle:step(Left),
    particle:step(Right),
    [Left, Right].

particle_moves_test_() ->
    {"a particle in isolation follows its own velocity",
      {setup,
       fun run_both_particles_separately/0,
       fun (Particles) ->
               [check_particle_moves(Particle) || Particle <- Particles]
       end}}.

check_particle_moves(Particle) ->
    Position = particle:get_position(Particle),
    ok = particle:step(Particle),
    ?_assertNotEqual(Position, particle:get_position(Particle)).
