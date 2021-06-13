-module(test_particle_server).

-include_lib("eunit/include/eunit.hrl").

start_servers() ->
    Abs = fun([X]) -> abs(X) end,
    {ok, Server1} = particle_server:start_link(
                      [10.0],
                      [-1.0],
                      Abs,
                      [{bounds, {[-10.0], [10.0]}}]),
    {ok, Server2} = particle_server:start_link(
                      [-10.0],
                      [1.0],
                      Abs,
                      [{bounds, {[-10.0], [10.0]}}]),
    {Server1, Server2}.

stop_servers({Server1, Server2}) ->
    particle_server:stop(Server1),
    particle_server:stop(Server2).

single_step_test_() ->
    {setup,
     fun start_servers/0,
     fun stop_servers/1,
     fun({ServerA, ServerB}) ->
             {inorder,
              [{"after assigning neighbors no step is taken",
                fun() ->
                        InitialStateA = particle_server:state(ServerA),
                        InitialStateB = particle_server:state(ServerB),
                        particle_server:set_neighbors(ServerA, [ServerB]),
                        particle_server:set_neighbors(ServerB, [ServerA]),
                        ?assertEqual(InitialStateA, particle_server:state(ServerA)),
                        ?assertEqual(InitialStateB, particle_server:state(ServerB))
                end},
               {"after one step both particles move toward 0.0",
                fun() ->
                        {ok, 2} = particle_server:eval(ServerA, 1),
                        {[PositionA], _} = particle_server:state(ServerA, 2),
                        {[PositionB1], _} = particle_server:state(ServerB, 1),
                        ?assert(PositionA =< 10.0),
                        ?assertEqual(-10.0, PositionB1),
                        {ok, 2} = particle_server:eval(ServerB, 1),
                        {[PositionB], _} = particle_server:state(ServerB, 2),
                        ?assert(PositionB >= -10.0)
                end},
              {"after more iterations the particles move closer to 0.0",
               fun() ->
                       {[StartingA], _} = particle_server:state(ServerA),
                       {[StartingB], _} = particle_server:state(ServerB),
                       {ok, 502} = particle_server:eval(ServerA, 500),
                       {ok, 502} = particle_server:eval(ServerB, 500),
                       {[EndingA], _} = particle_server:state(ServerA, 502),
                       {[EndingB], _} = particle_server:state(ServerB, 502),
                       ?assert(abs(EndingA) < abs(StartingA)),
                       ?assert(abs(EndingB) < abs(StartingB))
               end},
              {"the final values are the output of the objective function",
               fun() ->
                       {[A], ValueA} = particle_server:state(ServerA),
                       {[B], ValueB} = particle_server:state(ServerB),
                       ?assertEqual(abs(A), ValueA),
                       ?assertEqual(abs(B), ValueB)
               end}]}
     end}.

stop_while_iterating_test() ->
    {setup,
     fun() ->
             {ServerA, ServerB} = start_servers(),
             particle_server:set_neighbors(ServerA, [ServerB]),
             particle_server:set_neighbors(ServerB, [ServerA]),
             {ServerA, ServerB}
     end,
     fun({ServerA, ServerB}) ->
             fun() ->
                     particle_server:eval(ServerA, 1000000000000000),
                     particle_server:eval(ServerB, 1000000000000000),
                     particle_server:stop(ServerA),
                     particle_server:stop(ServerB),
                     ?assertNot(is_process_alive(ServerA)),
                     ?assertNot(is_process_alive(ServerB))
             end
     end}.
