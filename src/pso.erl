-module(pso).

-export([start_particles_link/4]).

%% @doc
%%
%% Start a particle at each position and velocity in
%% `InitialPositions' and `InitialVelocities'. The particles are
%% connected as specified in `Network'. The nodes in the network are
%% numberd according to the index of the particles in
%% `InitialPositions' and `InitialVelocities'.
%%
%% @end
-spec start_particles_link(InitialPositions :: list(term()),
                           InitialVelocities :: list(term()),
                           Network :: network:network(),
                           Module :: module()) ->
          {ok, Particles :: list(pid())}.
start_particles_link(InitialPositions, InitialVelocities, Network, Module) ->
    Particles =
        [particle:start_link(Position, Velocity, Module) ||
            {Position, Velocity} <- lists:zip(
                                      InitialPositions, InitialVelocities)],
    assign_neighbors(Network, Particles),
    Particles.

-spec assign_neighbors(Network :: network:network(),
                       Particles :: list(pid())) -> ok.
assign_neighbors(Network, Particles) ->
    ParticleMap = maps:from_list(lists:zip(lists:seq(1, length(Particles)))),
    network:foreach_node(
      fun (Node) ->
              particle:assign_neighbors(
                maps:get(Node, ParticleMap),
                network:neighbors(
                  Network, network:get_neighbors(Node, Network)))
      end, Network).
