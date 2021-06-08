-module(pso).

-export([run/5]).

-type position() :: particle:position().
-type value() :: particle:value().

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
          Iterations :: pos_integer()) -> particle:state().
run(ObjectiveFun, InitialPositions, InitialVelocities, Network, Iterations) ->
    Particles = maps:from_list(
                  [{N,
                    element(2, particle_server:start_link(Pos, Vel, ObjectiveFun, []))}
                   || {N, Pos, Vel} <- lists:zip3(network:all_nodes(Network),
                                                  InitialPositions,
                                                  InitialVelocities)]),
    %% Set up the neighbors.
    maps_foreach(
      fun (N, ParticleServer) ->
              particle_server:set_neighbors(
                ParticleServer,
                [maps:get(Neighbor, Particles)
                 || Neighbor <- network:neighbors(N, Network)])
      end, Particles),

    %% Kick off the iteration.
    maps_foreach(
      fun (_, ParticleServer) ->
              particle_server:eval(ParticleServer, Iterations)
      end, Particles),

    %% Get the best solution
    Best = best_solution(maps:values(Particles), Iterations),

    %% Stop the servers
    lists:foreach(fun particle_server:stop/1, maps:values(Particles)),

    Best.


-spec maps_foreach(Fun :: fun((Key, Value) -> any()),
                   Map :: #{Key := Value}) -> any().
maps_foreach(Fun, Map) ->
    maps:fold(fun (Key, Value, _) -> Fun(Key, Value) end, nil, Map).

-spec best_solution(Particles :: [pid()], After :: pos_integer())
                   -> particle:state().
best_solution(Particles, After) ->
    hd(lists:keysort(2, [particle_server:state(Particle, After)
                         || Particle <- Particles])).
