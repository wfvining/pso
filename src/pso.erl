-module(pso).

-export([run/5]).

-type position() :: particle:position().
-type value() :: particle:value().
-type particle() :: particle:particle().

%% @doc
%%
%% Run `Iterations' iterations of the PSO algorithm. The position and
%% value of the best solution is returned.
%%
%% @end
-spec run(ObjectiveFun :: fun((position()) -> value()),
          InitialPositions :: [position()],
          InitialVelocities :: [particle:velocity()],
          Network :: network:network(),
          Iterations :: pos_integer()) -> {position(), value()}.
run(ObjectiveFun, InitialPositions, InitialVelocities, Network, Iterations) ->
    Particles = maps:from_list(
                  [{N, particle:new(Pos, Vel, ObjectiveFun)}
                   || {N, Pos, Vel} <- lists:zip3(network:all_nodes(Network),
                                                  InitialPositions,
                                                  InitialVelocities)]),
    best_solution(maps:values(iterate(Particles, Network, Iterations))).

-spec best_solution(Particles :: [particle()]) -> {position(), value()}.
best_solution(Particles) ->
    BestParticle =
        hd(lists:sort(
             fun(ParticleA, ParticleB) ->
                     particle:value(ParticleA) < particle:value(ParticleB)
             end,
             Particles)),
    {particle:position(BestParticle), particle:value(BestParticle)}.

-spec iterate(Particles :: #{term() := particle()},
              Network :: network:network(),
              Iterations :: non_neg_integer()) -> #{term() := particle()}.
iterate(Particles, _, 0) ->
    Particles;
iterate(Particles, Network, Iterations) ->
    NewParticles =
        maps:map(
          fun(N, Particle) ->
                  particle:step(Particle,
                                [particle:state(maps:get(P, Particles))
                                 || P <- network:neighbors(N, Network)])
          end, Particles),
    iterate(NewParticles, Network, Iterations - 1).
