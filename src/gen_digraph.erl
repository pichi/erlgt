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
        , vertices/1
        , in_neighbours/2
        , out_neighbours/2
        , sources/1
        , sinks/1
        , delete/1
        ]).

-export([ gen_vertices/1
        , gen_sources/1
        , gen_sinks/1
        ]).

-ifdef(TEST).

-include_lib("proper/include/proper.hrl").

-export([ digraph/0
        , acyclic_digraph/0
        , prop_edgelist/1
        , prop_vertices/1
        , prop_sinks/1
        , prop_sources/1
        ]).

-endif.

%% -----------------------------------------------------------------------------
%% Behaviour
%% -----------------------------------------------------------------------------

-type gen_digraph() :: {atom(), term()}.

-type vertice() :: term().

-callback from_edgelist([{vertice(), vertice()}]) -> gen_digraph().

-callback to_edgelist(Graph :: gen_digraph()) -> [{vertice(), vertice()}].

-callback vertices(Graph :: gen_digraph()) -> [vertice()].

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

vertices(?G) -> M:vertices(G).

in_neighbours(?G, V) -> M:in_neighbours(G, V).

out_neighbours(?G, V) -> M:out_neighbours(G, V).

sources(?G) -> M:sources(G).

sinks(?G) -> M:sinks(G).

delete(?G) -> M:delete(G).

%% -----------------------------------------------------------------------------
%% Generic implementations
%% -----------------------------------------------------------------------------

gen_vertices(G) ->
    % Note sources and sinks can has implementation different form gen_* so
    % it can return vertices in any particular order.
    lists:umerge([ lists:usort(Vs) || Vs <- [sources(G), sinks(G)] ]).

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

-define(WITH_G(L, Do),
        begin
            G = Module:from_edgelist(L),
            try Do after delete(G)
            end
        end
       ).

-define(WITH_G(L, DoEmpty, DoOther),
        ?WITH_G(L,
                case L of
                    [] -> DoEmpty;
                    _ -> DoOther
                end
               )
       ).

prop_edgelist(Module) ->
    ?FORALL(
       L, digraph(),
       ?WITH_G(
          L, equals(lists:usort(L),lists:usort(to_edgelist(G)))
         )
      ).

prop_vertices(Module) ->
    ?FORALL(
       L, digraph(),
       ?WITH_G(
          L, equals([], vertices(G)),
          ?FORALL(
             {V1, V2}, oneof(L),
             case vertices(G) of
                 Vs ->
                     conjunction(
                       [{source, lists:member(V1, Vs)},
                        {sink, lists:member(V2, Vs)}]
                      )
             end
            )
         )
      ).

prop_sources(Module) ->
    ?FORALL(
       L, digraph(),
       ?WITH_G(
          L, equals([], sources(G)),
          ?FORALL(
             {V, _}, oneof(L),
             lists:member(V, sources(G))
            )
         )
      ).

prop_sinks(Module) ->
    ?FORALL(
       L, digraph(),
       ?WITH_G(
          L, equals([], sinks(G)),
          ?FORALL(
             {_, V}, oneof(L),
             lists:member(V, sinks(G))
            )
         )
      ).

-endif. %% TEST
