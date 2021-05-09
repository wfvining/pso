-module(vector).

-export([magnitude/1, add/2, subtract/2, multiply/2, scale_to/2]).

-export_type([vector/0]).

-type vector() :: [float()].

%% @doc Return the magnitude of a vector.
-spec magnitude(V :: vector()) -> float().
magnitude(V) ->
    math:sqrt(lists:sum([X*X || X <- V])).

%% @doc Return `X' plus `Y'.
add(X, Y) ->
    [Xi + Yi || {Xi, Yi} <- lists:zip(X, Y)].

%% @doc Return `X' minus `Y'.
-spec subtract(X :: vector(), Y :: vector()) -> vector().
subtract(X, Y) ->
    [Xi - Yi || {Xi, Yi} <- lists:zip(X, Y)].

%% @doc Scalar-vector multiplication.
-spec multiply(Scale :: float(), X :: vector()) -> vector().
multiply(Scale, X) ->
    [Scale * Xi || Xi <- X].

%% @doc
%% Return a new vector in the same direction as `X' with length
%% `Length'.
%% @end
-spec scale_to(Length :: float(), X :: vector()) -> vector().
scale_to(Length, X) ->
    multiply(Length / magnitude(X), X).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

add_test() ->
    ?assertEqual([0, 0, 0], add([1, 2, 3], [-1, -2, -3])).

multiply_test_() ->
    [?_assertEqual([0.0, 0.0, 0.0],
                   multiply(0.0, [1.0, 2.0, 3.0])),
     ?_assertEqual([1.0, 2.0, 3.0],
                   multiply(1.0, [1.0, 2.0, 3.0])),
     ?_assertEqual([-3.0, -4.0, 7.0],
                   multiply(2.0, [-1.5, -2, 3.5]))].

scale_to_test_() ->
    [?_assertEqual([0.0, 1.0], scale_to(1, [0.0, 1.5])),
     ?_assertEqual([0.0, 1.0], scale_to(1, [0.0, 0.5])),
     ?_assertEqual([1.0, 0.0], scale_to(1, [1.3, 0.0])),
     ?_assertEqual([1.0, 0.0], scale_to(1, [0.1, 0.0]))].

-endif.
