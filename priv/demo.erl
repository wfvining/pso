-module(demo).

-behavior(particle).

-export([eval/1, move/2, displacement/2, scale_velocity/2,
         add_velocity/2, compare/2]).

ackley(X, Y) ->
    -20*math:exp(
          -0.2 * math:sqrt(
                   0.5 * (X * X + Y * Y)))
        - math:exp(
            0.5 * (math:cos(
                     2.0 * math:pi() * X)
                   + math:cos(
                      2.0 * math:pi() * Y)))
         + math:exp(1) + 20.0.

eval({X, Y}) ->
    ackley(X, Y).

move({X, Y}, {VX, VY}) ->
    {X + VX, Y + VY}.

displacement({X1, Y1}, {X2, Y2}) ->
    DX = X1 - X2,
    DY = Y1 - Y2,
    math:sqrt(DX*DX + DY*DY).

scale_velocity(Scale, {X, Y}) ->
    {Scale * X, Scale * Y}.

add_velocity({X1, Y1}, {X2, Y2}) ->
    {X1 + X2, Y1 + Y2}.

compare(A, B) ->
    A =< B.
