%% -----------------------------------------------------------------------------
%%
%% The MIT License (MIT)
%%
%% Copyright (c) 2015 Hynek Vychodil
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%
%% -----------------------------------------------------------------------------
%%
%% @author Hynek Vychodil <vychodil.hynek@gmail.com>
%% @doc Generic Digraph Algorithms and Behaviour
%%
%% Provides generic digraph algorithms and behaviour definition. The module also
%% provides wrappers for all callbacks to simplify polymorphism. For example
%% you can use `gen_digraph:vertices(G)' as virtual method dispatcher for
%% `your_digraph:vertices({your_digraph, _} = G)'.
%%
%% Callback `from_edgelist` is required as simplest constructor for testing.
%%
%% @copyright 2015 Hynek Vychodil
%% @end
%%
%% -----------------------------------------------------------------------------
-module(gen_digraph).

-export_type([gen_digraph/0, vertice/0]).

-export([ to_edgelist/1
        , no_edges/1
        , vertices/1
        , no_vertices/1
        , in_neighbours/2
        , out_neighbours/2
        , sources/1
        , sinks/1
        , delete/1
        ]).

-export([ gen_no_edges/1
        , gen_vertices/1
        , gen_no_vertices/1
        , gen_in_neighbours/2
        , gen_out_neighbours/2
        , gen_sources/1
        , gen_sinks/1
        ]).

-ifdef(TEST).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([ digraph/0
        , acyclic_digraph/0
        , prop_edgelist/1
        , prop_no_edges/1
        , prop_vertices/1
        , prop_no_vertices/1
        , test_empty_vertices/1
        , prop_neighbours/1
        , gen_properties_tests/1
        , gen_properties_tests/2
        , gen_tests/1
        ]).

-endif.

%% -----------------------------------------------------------------------------
%% Behaviour
%% -----------------------------------------------------------------------------

-type gen_digraph() :: {atom(), term()}.

-type vertice() :: term().

-callback from_edgelist([{vertice(), vertice()}]) -> gen_digraph().

-callback to_edgelist(Graph :: gen_digraph()) -> [{vertice(), vertice()}].

-callback no_edges(Graph :: gen_digraph()) -> non_neg_integer().

-callback vertices(Graph :: gen_digraph()) -> [vertice()].

-callback no_vertices(Graph :: gen_digraph()) -> non_neg_integer().

-callback in_neighbours(Graph :: gen_digraph(), V :: vertice()) -> [vertice()].

-callback out_neighbours(Graph :: gen_digraph(), V :: vertice()) -> [vertice()].

-callback sources(Graph :: gen_digraph()) -> [vertice()].

-callback sinks(Graph :: gen_digraph()) -> [vertice()].

-callback delete(Graph :: gen_digraph()) -> true.

%% -----------------------------------------------------------------------------
%% Callback wrappers
%% -----------------------------------------------------------------------------

-define(G, {M, _} = G).

to_edgelist(?G) -> M:to_edgelist(G).

no_edges(?G) -> M:no_edges(G).

vertices(?G) -> M:vertices(G).

no_vertices(?G) -> M:no_vertices(G).

in_neighbours(?G, V) -> M:in_neighbours(G, V).

out_neighbours(?G, V) -> M:out_neighbours(G, V).

sources(?G) -> M:sources(G).

sinks(?G) -> M:sinks(G).

delete(?G) -> M:delete(G).

%% -----------------------------------------------------------------------------
%% Generic implementations
%% -----------------------------------------------------------------------------

gen_no_edges(G) ->
    length(to_edgelist(G)).

gen_vertices(G) ->
    % Note sources and sinks can has implementation different form gen_* so
    % it can return vertices in any particular order.
    lists:umerge([ lists:usort(Vs) || Vs <- [sources(G), sinks(G)] ]).

gen_no_vertices(G) ->
    length(vertices(G)).

gen_in_neighbours(G, V) ->
    lists:usort([ V1 || {V1, V2} <- to_edgelist(G), V2 =:= V ]).

gen_out_neighbours(G, V) ->
    lists:usort([ V2 || {V1, V2} <- to_edgelist(G), V1 =:= V ]).

gen_sources(G) ->
    lists:usort([ V1 || {V1, _} <- to_edgelist(G) ]).

gen_sinks(G) ->
    lists:usort([ V2 || {_, V2} <- to_edgelist(G) ]).

%% -----------------------------------------------------------------------------
%% Generic properties and generators
%% -----------------------------------------------------------------------------

-ifdef(TEST).

edge() -> ?SIZED(S, begin
                        V = round(math:sqrt(S)),
                        {integer(0, V), integer(1, V)}
                    end).

acyclic_edge() -> ?LET({A, B}, edge(), {A, A+B}).

digraph(Edge) -> list(Edge).

digraph() -> digraph(edge()).

acyclic_digraph() -> digraph(acyclic_edge()).

%% Warning! This macro can be used only as the innermost macro of a property
%% because it deletes graph `G'. In other words, If action `Do' returns
%% `test()', it has to perform all actions using `G' in direct code execution
%% of `Do'.
-define(WITH_G(L, Do),
        begin
            G = Module:from_edgelist(L),
            try Do after delete(G) end
        end
       ).

prop_edgelist(Module) ->
    ?FORALL(
       L, digraph(),
       ?WITH_G(
          L,
          begin
              El = lists:sort(to_edgelist(G)),
              Sl = lists:sort(L),
              Ul = lists:usort(Sl),
              ?WHENFAIL(
                 io:format("~p and ~p =/= ~p~n",
                           [Sl, Ul, El]),
                 Sl =:= El orelse Ul =:= El
                )
          end
         )
      ).

prop_no_edges(Module) ->
    ?FORALL(
       L, digraph(),
       ?WITH_G(L, equals(length(to_edgelist(G)), no_edges(G)))
      ).

prop_vertices(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       ?FORALL(
          {V1, V2}, oneof(L),
          ?WITH_G(L,
                  conjunction(
                    [{source,     lists:member(V1, vertices(G))},
                     {sink,       lists:member(V2, vertices(G))},
                     {in_sources, lists:member(V1, sources(G))},
                     {in_sinks,   lists:member(V2, sinks(G))}
                    ]
                   )
                 )
         )
      ).

prop_no_vertices(Module) ->
    ?FORALL(
       L, digraph(),
       ?WITH_G(L, equals(length(vertices(G)), no_vertices(G)))
      ).

test_empty_vertices(Module) ->
    ?_test(
       ?WITH_G(
          [],
          begin
              ?assertEqual([], vertices(G)),
              ?assertEqual([], sources(G)),
              ?assertEqual([], sinks(G))
          end
         )
      ).

prop_neighbours(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       ?FORALL(
          {V1, V2}, oneof(L),
          ?WITH_G(
             L,
             conjunction(
               [{in,  lists:member(V1,  in_neighbours(G, V2))},
                {out, lists:member(V2, out_neighbours(G, V1))}]
              )
            )
         )
      ).

gen_properties_tests(Module) ->
    gen_properties_tests(Module, []).

gen_properties_tests(Module, Opts) ->
    [{atom_to_list(X), ?_assert(proper:quickcheck(Prop, Opts))}
     || X <- [  prop_edgelist
              , prop_no_edges
              , prop_vertices
              , prop_no_vertices
              , prop_neighbours
             ],
        Prop <- [?MODULE:X(Module)]
    ].

gen_tests(Module) ->
    [{atom_to_list(X), Test}
     || X <- [test_empty_vertices],
        Test <- [?MODULE:X(Module)]
    ].


-endif. %% TEST
