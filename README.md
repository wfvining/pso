# Particle Swarm Optimization

An OTP library for particle swarm optimization. The library consists of
a module implementing the PSO algorithm for a single particle, as well as
a server that can be used to evaluate a PSO problem in parallel. The server
implements a concurrent mechanism for exchanging state between particles and
their neighbors. This mechanism supports evaluation of the PSO algorithm
without central control over the iteration.

## A PSO Problem

A problem is specified by a collection of points, velocities, an objective
function, and an interaction network. A point and a velocity are both list of
floats, the objective function maps points to a single float value. Particle
swarm optimization attempts to find the point which minimizes this objective
function. The interaction network specifies the neighbors of every particle,
it should consist of a single connected component. The names of the nodes
in the network may be arbitrary and are arbitrarily mapped to the points and
velocities.

## Usage

### Build


    $ rebar3 compile

### Run

```erlang
NumParticles = 50

Sphere = fun(X) -> lists:sum([Xi*Xi || Xi <- X]) end,

% random points in four dimensions
Points = [[(rand:uniform() * 100) - 50 || _ <- lists:seq(1, 4)]
          || _ <- lists:seq(1, NumParticles)],
Velocities = [[(rand:uniform() * 10) - 5 || _ <- Point]
              || Point <- Points],

% make a ring
Network = network:ring(50, 5),

% run 1000 iterations
pso:run(Sphere, Points, Velocities, Network, 1000)
```

## To Do

What are the next steps? Where do I want to take this?

- [ ] Configurable random seeds for particles
- [ ] Checkpoints - Is it worth being able to checkpoint and restore
      the state of the particles? This could be useful for recovering
      when the evaluation function crashes the particle server.
- [ ] Alternative evaluators - I would like to be able to configure a
      process pool to use for the evaluation of the particles. I have
      a few ideas about this (beyond a simple pool, could do some kind
      of distributed load-balancing process pool as well).
- [ ] Should we supervise the particle servers? Could build out a
      supervisor for the particle servers, but it isn't clear to me
      that this would really be all that helpful. We pretty much want
      to fail the run if one of them crashes, and they aren't meant to
      be long-lived anyway. What is the benefit of supervision in this
      case? Well, if we supervised `one_for_one`, we could recover
      from sporadic failures without sacrificing the
      computation/progress that has been made so far.
