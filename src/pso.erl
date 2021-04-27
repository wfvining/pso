-module(pso).

-export([run/3]).

-type position() :: particle:position().
-type value() :: term().

-spec run(InitialPositions :: [position()],
          Network :: network:network(),
          Iterations :: pos_integer()) -> {position(), value()}.
run(_, _, _) -> undefined.
