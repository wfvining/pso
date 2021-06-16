%%% @author Will Vining <wfvining@gmail.com>
%%% @copyright (C) 2021, Will Vining
%%% @doc
%%% Basic associative network.
%%% @end
%%% Created : 20 Feb 2021 by Will Vining <wfvining@gmail.com>

-module(network).

-export([empty/0, empty/1, from_edges/1, from_edges/2, ring/2]).
-export([neighbors/2, all_nodes/1, all_edges/1, foreach_node/2]).

-record(network, {type = undirected :: edge_type(),
                  adjacency_list = #{} :: #{term() => gb_sets:set()}}).

-export_type([network/0]).

-type network() :: #network{}.
-type edge() :: {term(), term()}.
-type edge_type() :: undirected | directed.

%% @doc Create an empty undirected network.
-spec empty() -> network().
empty() ->
    empty(undirected).

%% @doc Create an empty network of the given `Type'.
-spec empty(Type :: edge_type()) -> network().
empty(undirected) ->
    #network{
       type = undirected,
       adjacency_list = #{}}.

%% @doc Create an undirected network from a list of edges.
-spec from_edges(EdgeList :: list(edge())) -> network().
from_edges(EdgeList) ->
    from_edges(EdgeList, undirected).

%% @doc
%% Create a network of type `Type' from a list of edges.
%% @end
-spec from_edges(EdgeList :: list(edge()), Type :: edge_type()) -> network().
from_edges(EdgeList, undirected) ->
    #network{
       type = undirected,
       adjacency_list =
           lists:foldl(
             fun ({U, V}, AdjacencyList) ->
                     maps:update_with(
                       U,
                       fun(Set) -> gb_sets:add_element(V, Set) end,
                       gb_sets:from_list([V]),
                       maps:update_with(
                         V,
                         fun(Set) -> gb_sets:add_element(U, Set) end,
                         gb_sets:from_list([U]),
                         AdjacencyList))
             end, #{}, EdgeList)}.

%% @doc Return the neighbors of `Node'.
-spec neighbors(Node :: term(), Network :: network()) -> list(term()).
neighbors(Node, #network{adjacency_list = AdjacencyList}) ->
    gb_sets:to_list(maps:get(Node, AdjacencyList)).

%% @doc Return a list of all nodes in `Network'.
-spec all_nodes(Network :: network()) -> list(term()).
all_nodes(#network{adjacency_list = AdjacencyList}) ->
    maps:keys(AdjacencyList).

%% @doc Return a list of all edges in `Network'.
-spec all_edges(Network :: network()) -> list(edge()).
all_edges(#network{adjacency_list = AdjacencyList}) ->
    maps:fold(
      fun(Node, Neighbors, Edges) ->
              [{Node, N} || N <- gb_sets:to_list(Neighbors), Node < N] ++ Edges
      end, [], AdjacencyList).

%% @doc Apply `Fun' to each node in `Network'.
-spec foreach_node(Fun :: fun((term()) -> term()), Network :: network()) -> ok.
foreach_node(Fun, Network) ->
    lists:foreach(Fun, maps:keys(Network#network.adjacency_list)).

%% @doc Construct a ring network with `NumNodes' nodes and `Radius'.
-spec ring(NumNodes :: pos_integer(), Radius :: pos_integer()) -> network().
ring(NumNodes, Radius) ->
    Edges = lists:flatten(
              [[{X, Y rem NumNodes} || Y <- lists:seq(X+1, X + Radius)]
               || X <- lists:seq(0, NumNodes - 1)]),
    network:from_edges(Edges).
