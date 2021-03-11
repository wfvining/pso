-module(network_tests).

-include_lib("eunit/include/eunit.hrl").

from_list_empty_test() ->
    EmptyNetwork = network:from_edges([]),
    ?assertEqual([], network:all_nodes(EmptyNetwork)),
    ?assertEqual([], network:all_edges(EmptyNetwork)).

from_list_one_edge_test() ->
    Network = network:from_edges([{1, 2}]),
    ?assertEqual(sets:from_list([1, 2]),
                 sets:from_list(network:all_nodes(Network))),
    ?assertEqual([{1, 2}], network:all_edges(Network)).

from_list_three_test() ->
    Network = network:from_edges([{1, 2}, {3, 1}]),
    ?assertEqual(sets:from_list([1, 2, 3]),
                 sets:from_list(network:all_nodes(Network))),
    ?assertEqual(sets:from_list([{1, 2}, {1, 3}]),
                 sets:from_list(network:all_edges(Network))).

neighbors_test() ->
    Network = network:from_edges([{1, 2}, {3, 1}, {1, 4}, {2, 4}]),
    ?assertEqual(sets:from_list([1, 4]),
                 sets:from_list(network:neighbors(2, Network))),
    ?assertEqual(sets:from_list([2, 3, 4]),
                 sets:from_list(network:neighbors(1, Network))).
