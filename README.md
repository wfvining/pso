pso
=====

An OTP library for particle swarm optimization. The library consists of
a module implementing the PSO algorithm for a single particle, as well as
a server that can be used to evaluate a PSO problem in parallel. The server
implements a concurrent mechanism for exchanging state between particles and
their neighbors. This mechanism supports evaluation of the PSO algorithm
without central control over the iteration.

A PSO Problem
-------------

A problem is specified by a collection of points, velocities, an objective
function, and an interaction network. A point and a velocity are both list of
floats, the objective function maps points to a single float value. Particle
swarm optimization attempts to find the point which minimizes this objective
function. The interaction network specifies the neighbors of every particle,
it should consist of a single connected component. The names of the nodes
in the network may be arbitrary and are arbitrarily mapped to the points and
velocities.

Build
-----

    $ rebar3 compile

Run
---

```erlang
NumParticles = 50
Sphere = fun(X) -> lists:sum([Xi*Xi || Xi <- X]) end,
% random points in four dimensions
Points = [[(rand:uniform() * 100) - 50 || _ <- lists:seq(1, 4)] 
          || _ <- lists:seq(1, NumParticles)],
Velocities = [[(rand:uniform() * 10) - 5 || _ <- Point] 
              || Point <- Points],
% make a ring
Network = network:from_edges(
              lists:flatten([[{X, Y rem 5} || Y <- lists:seq(X, X+5)]
                             || X <- lists:seq(0, 49)])),
% run 1000 iterations
pso:run(Sphere, Points, Velocities, Network, 1000)
```
