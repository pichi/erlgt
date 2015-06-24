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
%% Callback `from_list` is required as simplest constructor for testing.
%%
%% @copyright 2015 Hynek Vychodil
%% @end
%%
%% -----------------------------------------------------------------------------
-module(gen_digraph).

-export_type([gen_digraph/0, vertex/0]).

-export([ to_list/1
        , no_edges/1
        , vertices/1
        , no_vertices/1
        , in_neighbours/2
        , out_neighbours/2
        , in_degree/2
        , out_degree/2
        , sources/1
        , sinks/1
        , delete/1
        , has_edge/3
        , has_path/2
        , get_path/3
        , get_cycle/2
        , get_short_path/3
        , get_short_cycle/2
        , reachable/2
        , reachable_neighbours/2
        , reaching/2
        , reaching_neighbours/2
        , components/1
        , strong_components/1
        , preorder/1
        , is_acyclic/1
        , postorder/1
        , topsort/1
        ]).

-export([ gen_no_edges/1
        , gen_vertices/1
        , gen_no_vertices/1
        , gen_in_neighbours/2
        , gen_out_neighbours/2
        , gen_in_degree/2
        , gen_out_degree/2
        , gen_sources/1
        , gen_sinks/1
        , gen_has_edge/3
        , gen_has_path/2
        , gen_get_path/3
        , gen_get_path_lists/3
        , gen_get_path_maps/3
        , gen_get_cycle/2
        , gen_get_short_path/3
        , gen_get_short_path_maps/3
        , gen_get_short_path_lists/3
        , gen_get_short_cycle/2
        , gen_reachable/2
        , gen_reachable_neighbours/2
        , gen_reaching/2
        , gen_reaching_neighbours/2
        , gen_components/1
        , gen_strong_components/1
        , gen_preorder/1
        , gen_is_acyclic/1
        , gen_postorder/1
        , gen_topsort/1
        ]).

-ifdef(TEST).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([ digraph/0
        , acyclic_digraph/0
        , prop_list/1
        , prop_no_edges/1
        , prop_vertices/1
        , prop_no_vertices/1
        , test_empty_vertices/1
        , prop_neighbours/1
        , prop_has_edge/1
        , prop_has_path/1
        , prop_get_path/1
        , prop_get_cycle/1
        , prop_reachable/1
        , prop_components/1
        , prop_preorder/1
        , prop_is_acyclic/1
        , prop_postorder/1
        , prop_topsort/1
        , gen_properties_tests/1
        , gen_properties_tests/2
        , gen_tests/1
        ]).

-endif.

%% -----------------------------------------------------------------------------
%% Behaviour
%% -----------------------------------------------------------------------------

-type gen_digraph() :: {atom(), term()}.

-type vertex() :: term().

-callback from_list([{vertex(), vertex()}]) -> gen_digraph().

-callback to_list(Graph :: gen_digraph()) -> [{vertex(), vertex()}].

-callback no_edges(Graph :: gen_digraph()) -> non_neg_integer().

-callback vertices(Graph :: gen_digraph()) -> [vertex()].

-callback no_vertices(Graph :: gen_digraph()) -> non_neg_integer().

-callback in_neighbours(Graph :: gen_digraph(), V :: vertex()) -> [vertex()].

-callback out_neighbours(Graph :: gen_digraph(), V :: vertex()) -> [vertex()].

-callback in_degree(Graph :: gen_digraph(), V :: vertex()) -> non_neg_integer().

-callback out_degree(Graph :: gen_digraph(), V :: vertex()) -> non_neg_integer().

-callback sources(Graph :: gen_digraph()) -> [vertex()].

-callback sinks(Graph :: gen_digraph()) -> [vertex()].

-callback delete(Graph :: gen_digraph()) -> true.

-callback has_edge(Graph :: gen_digraph(), V1 :: vertex(), V2 :: vertex()) ->
    boolean().

-callback has_path(Graph :: gen_digraph(), [V1 :: vertex()]) -> boolean().

-callback get_path(Graph :: gen_digraph(), V1 :: vertex(), V2 :: vertex()) ->
    [vertex()] | false.

-callback get_cycle(Graph :: gen_digraph(), V :: vertex()) -> [vertex()] | false.

-callback get_short_path(Graph :: gen_digraph(), V1 :: vertex(), V2 :: vertex()) ->
    [vertex()] | false.

-callback get_short_cycle(Graph :: gen_digraph(), V :: vertex()) ->
    [vertex()] | false.

-callback reachable(Graph :: gen_digraph(), Vs :: [vertex()]) -> [vertex()].

-callback reachable_neighbours(Graph :: gen_digraph(), Vs :: [vertex()]) ->
    [vertex()].

-callback reaching(Graph :: gen_digraph(), Vs :: [vertex()]) -> [vertex()].

-callback reaching_neighbours(Graph :: gen_digraph(), Vs :: [vertex()]) ->
    [vertex()].

-callback components(Graph :: gen_digraph()) -> [[vertex()]].

-callback strong_components(Graph :: gen_digraph()) -> [[vertex()]].

-callback preorder(Graph :: gen_digraph()) -> [vertex()].

-callback is_acyclic(Graph :: gen_digraph()) -> boolean().

-callback postorder(Graph :: gen_digraph()) -> [vertex()].

-callback topsort(Graph :: gen_digraph()) -> [vertex()] | false.

%% -----------------------------------------------------------------------------
%% Callback wrappers
%% -----------------------------------------------------------------------------

-define(G, {M, _} = G).

to_list(?G) -> M:to_list(G).

no_edges(?G) -> M:no_edges(G).

vertices(?G) -> M:vertices(G).

no_vertices(?G) -> M:no_vertices(G).

in_neighbours(?G, V) -> M:in_neighbours(G, V).

out_neighbours(?G, V) -> M:out_neighbours(G, V).

in_degree(?G, V) -> M:in_degree(G, V).

out_degree(?G, V) -> M:out_degree(G, V).

sources(?G) -> M:sources(G).

sinks(?G) -> M:sinks(G).

delete(?G) -> M:delete(G).

has_edge(?G, V1, V2) -> M:has_edge(G, V1, V2).

has_path(?G, P) -> M:has_path(G, P).

get_path(?G, V1, V2) -> M:get_path(G, V1, V2).

get_cycle(?G, V) -> M:get_cycle(G, V).

get_short_path(?G, V1, V2) -> M:get_short_path(G, V1, V2).

get_short_cycle(?G, V) -> M:get_short_cycle(G, V).

reachable(?G, V) -> M:reachable(G, V).

reachable_neighbours(?G, V) -> M:reachable_neighbours(G, V).

reaching(?G, V) -> M:reaching(G, V).

reaching_neighbours(?G, V) -> M:reaching_neighbours(G, V).

components(?G) -> M:components(G).

strong_components(?G) -> M:strong_components(G).

preorder(?G) -> M:preorder(G).

is_acyclic(?G) -> M:is_acyclic(G).

postorder(?G) -> M:postorder(G).

topsort(?G) -> M:topsort(G).

%% -----------------------------------------------------------------------------
%% Generic implementations
%% -----------------------------------------------------------------------------

gen_no_edges(G) ->
    length(to_list(G)).

gen_vertices(G) ->
    % Note sources and sinks can has implementation different form gen_* so
    % it can return vertices in any particular order.
    lists:umerge([ lists:usort(Vs) || Vs <- [sources(G), sinks(G)] ]).

gen_no_vertices(G) ->
    length(vertices(G)).

gen_in_neighbours(G, V) ->
    lists:usort([ V1 || {V1, V2} <- to_list(G), V2 =:= V ]).

gen_out_neighbours(G, V) ->
    lists:usort([ V2 || {V1, V2} <- to_list(G), V1 =:= V ]).

gen_in_degree(G, V) ->
    length(in_neighbours(G, V)).

gen_out_degree(G, V) ->
    length(out_neighbours(G, V)).

gen_sources(G) ->
    lists:usort([ V1 || {V1, _} <- to_list(G) ]).

gen_sinks(G) ->
    lists:usort([ V2 || {_, V2} <- to_list(G) ]).

gen_has_edge(G, V1, V2) ->
    lists:member({V1, V2}, to_list(G)).

gen_has_path(G, [V|P]) -> gen_has_path(G, V, P).

gen_has_path(_, _, []) -> true;
gen_has_path(G, V1, [V2|P]) ->
    has_edge(G, V1, V2) andalso gen_has_path(G, V2, P).

gen_get_path(G, V1, V2) ->
    gen_get_path_maps(G, V1, V2).

gen_get_path_lists(G, V1, V2) ->
    get_one_path(G, V2, [], out_neighbours(G, V1), [V1], [V1]).

gen_get_path_maps(G, V1, V2) ->
    get_one_path(G, V2, [], out_neighbours(G, V1), maps:from_list([{V1, []}]), [V1]).

gen_get_cycle(G, V) -> get_path(G, V, V).

gen_get_short_path(G, V1, V2) ->
    gen_get_short_path_maps(G, V1, V2).

gen_get_short_path_maps(G, V1, V2) ->
    get_one_short_path(G, V2, [[V1]], [], #{}).

gen_get_short_path_lists(G, V1, V2) ->
    get_one_short_path(G, V2, [[V1]], [], []).

gen_get_short_cycle(G, V) -> get_short_path(G, V, V).

gen_reachable(G, Vs) ->
    revpreorder(out(G), Vs).

gen_reachable_neighbours(G, Vs) ->
    Out = out(G),
    revpreorder(Out, [V || X <- Vs, V <- Out(X) ]).

gen_reaching(G, Vs) ->
    revpreorder(in(G), Vs).

gen_reaching_neighbours(G, Vs) ->
    In = in(G),
    revpreorder(In, [V || X <- Vs, V <- In(X) ]).

gen_components(G) ->
    forest(inout(G), vertices(G)).

gen_strong_components(G) ->
    forest(in(G), revpostorder(G)).

gen_preorder(G) ->
    lists:reverse(revpreorder(out(G), vertices(G))).

gen_is_acyclic(G) ->
    Loop = fun(V) -> has_edge(G, V, V) end,
    not lists:any(Loop, vertices(G)) andalso not has_long_cycle(G).

gen_postorder(G) ->
    lists:reverse(revpostorder(G)).

gen_topsort(G) ->
    L = revpostorder(G),
    not has_long_cycle(G, L, #{}) andalso L.

-spec get_one_path(Graph :: gen_digraph(), Traget :: vertex(),
                   Stack :: [ToDo :: [vertex()]],
                   Neighbours :: [vertex()],
                   Seen  :: [vertex()], Path :: [vertex()]) ->
    [vertex()] | false.
get_one_path(_, T, _, [T|_], _, P) -> lists:reverse(P, [T]);
get_one_path(G, T, S, [V|Ns], Seen, P) ->
    case seen(V, Seen) of
        true  -> get_one_path(G, T, S, Ns, Seen, P);
        false ->
            S2  = [Ns|S],
            Ns2 = out_neighbours(G, V),
            get_one_path( G, T, S2, Ns2, add2seen(V,Seen), [V|P])
    end;
get_one_path(G, T, [Ns|S], [], Seen, P) ->
    get_one_path(G, T, S, Ns, Seen, tl(P));
get_one_path(_, _, [], [], _, _) -> false.

get_one_short_path(G, T, [[V|_]=P|Current], Next, Seen) ->
    get_one_short_path(G, T, Current, Next, Seen, P, out_neighbours(G, V));
get_one_short_path(_, _, [], [], _) -> false;
get_one_short_path(G, T, [], Next, Seen) ->
    get_one_short_path(G, T, Next, [], Seen).

get_one_short_path(_, T, _, _, _, P, [T|_]) -> lists:reverse(P, [T]);
get_one_short_path(G, T, Current, Next, Seen, P, [V|Ns]) ->
    case seen(V, Seen) of
        true ->
            get_one_short_path(G, T, Current, Next, Seen, P, Ns);
        false ->
            get_one_short_path(G, T, Current, [[V|P]|Next],
                               add2seen(V, Seen), P, Ns)
    end;
get_one_short_path(G, T, Current, Next, Seen, _, []) ->
    get_one_short_path(G, T, Current, Next, Seen).

revpreorder(F, Vs) ->
    element(1, ptraverse(F, Vs, #{}, [])).

ptraverse(F, [V|Vs], Seen, Acc) ->
    case seen(V, Seen) of
        true  ->
            ptraverse(F, Vs, Seen, Acc);
        false ->
            NewSeen = add2seen(V, Seen),
            ptraverse(F, not_seen(NewSeen, F(V), Vs), NewSeen, [V|Acc])
    end;
ptraverse(_, [], Seen, Acc) -> {Acc, Seen}.

revpostorder(G) ->
    posttraverse(G, vertices(G), [], #{}, []).

posttraverse(G, [V|Vs] = State, Stack, Seen, Acc) ->
    case seen(V, Seen) of
        true ->
            posttraverse(G, Vs, Stack, Seen, Acc);
        false ->
            Ns = not_seen(Seen, out_neighbours(G, V), []),
            posttraverse(G, Ns, [State|Stack], add2seen(V, Seen), Acc)
    end;
posttraverse(G, [], [[V|Vs]|Stack], Seen, Acc) ->
    posttraverse(G, Vs, Stack, Seen, [V|Acc]);
posttraverse(_, [], [], _, Acc) -> Acc.

forest(F, Vs) ->
    forest(F, Vs, #{}, []).

forest(_, [], _, Acc) -> Acc;
forest(F, [V|Vs], Seen, Acc) ->
    case ptraverse(F, [V], Seen, []) of
        {[], Seen} ->
            forest(F, Vs, Seen, Acc);
        {[_|_] = Component, NewSeen} ->
            forest(F, Vs, NewSeen, [Component|Acc])
    end.

seen(V, #{} = Seen) -> maps:is_key(V, Seen);
seen(V, Seen) -> lists:member(V, Seen).

add2seen(V, #{} = Seen) -> maps:put(V, [], Seen);
add2seen(V, Seen) -> [V|Seen].

not_seen(_, [], Acc) -> Acc;
not_seen(Seen, [V|Vs], Acc) ->
    case seen(V, Seen) of
        true  -> not_seen(Seen, Vs, Acc);
        false -> not_seen(Seen, Vs, [V|Acc])
    end.

out(G) ->
    fun(X) -> out_neighbours(G, X) end.

in(G) ->
    fun(X) -> in_neighbours(G, X) end.

inout(G) ->
    fun(X) -> in_neighbours(G, X) ++ out_neighbours(G, X) end.

has_long_cycle(G) ->
    has_long_cycle(G, revpostorder(G), #{}).

has_long_cycle(_, [], _) -> false;
has_long_cycle(G, [V|P], Seen) ->
    SeenF = fun(X) -> seen(X, Seen) end,
    lists:any(SeenF, out_neighbours(G, V)) orelse
    has_long_cycle(G, P, add2seen(V, Seen)).

%% -----------------------------------------------------------------------------
%% Generic properties and generators
%% -----------------------------------------------------------------------------

-ifdef(TEST).

edge() ->
    ?SIZED(S, begin
                  V = round(math:sqrt(S)),
                  {integer(0, V-1), integer(0, V)}
              end
          ).

acyclic_edge() -> ?LET({A, B}, edge(), {B, A+B+1}).

digraph(Edge) -> list(Edge).

digraph() -> digraph(edge()).

acyclic_digraph() -> digraph(acyclic_edge()).

twoof([X]) -> {X, X};
twoof(L) -> twoof(L, L).

twoof(L1, L2) ->
    ?SUCHTHATMAYBE({X, Y}, {oneof(L1), oneof(L2)}, X =/= Y).

is_simple_path(R, V1, V2, P) ->
    LoP = length(P),
    LoU = length(lists:usort(P)),
    case {hd(P), lists:last(P)} of
        {V1, V2} when V1 =:= V2 ->
            LoP =:= LoU + 1; % cycle
        {V1, V2} ->
            LoP =:= LoU;
        _ -> false
    end andalso has_path(R, P).

%% Warning! This macro can be used only as the innermost macro of a property
%% because it deletes graph `G'. In other words, If action `Do' returns
%% `test()', it has to perform all actions using `G' in direct code execution
%% of `Do'.
-define(WITH_G(L, Do),
        begin
            G = Module:from_list(L),
            try Do after delete(G) end
        end
       ).

prop_list(Module) ->
    ?FORALL(
       L, digraph(),
       ?WITH_G(
          L,
          begin
              El = lists:sort(to_list(G)),
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
       ?WITH_G(L, equals(length(to_list(G)), no_edges(G)))
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
                {out, lists:member(V2, out_neighbours(G, V1))},
                {in_degree,
                 equals(length(in_neighbours(G, V2)), in_degree(G, V2))},
                {out_degree,
                 equals(length(out_neighbours(G, V1)), out_degree(G, V1))}
               ]
              )
            )
         )
      ).

prop_has_edge(Module) ->
    ?FORALL(
       {{V1, V2} = E, L}, {edge(), digraph()},
       ?WITH_G(
          L,
          begin
              Expect = lists:member(E, L),
              collect(Expect, equals(Expect, has_edge(G, V1, V2)))
          end
         )
      ).

prop_has_path(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       begin
           R = list_digraph:from_list(L),
           ?FORALL(
              P, non_empty(list(oneof(vertices(R)))),
              ?WITH_G(
                 L,
                 begin
                     Expect = gen_has_path(R, P),
                     collect(Expect andalso {length(P), length(L)}, equals(Expect, has_path(G, P)))
                 end
                )
             )
       end
      ).

prop_get_path(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       begin
           R = list_digraph:from_list(L),
           ?FORALL(
              {V1, V2}, twoof(sources(R), sinks(R)),
              ?WITH_G(
                 L,
                 begin
                     Expect   = gen_get_path_lists(R, V1, V2),
                     S_Expect = gen_get_short_path_lists(R, V1, V2),
                     Path     = get_path(G, V1, V2),
                     S_Path   = get_short_path(G, V1, V2),
                     Classes  = case Expect of
                                    false -> [false];
                                    _ -> [{Expect =:= Path, length(Expect)},
                                          {S_Expect =:= S_Path,
                                           length(S_Expect), length(Expect)}]
                                end,
                     ?WHENFAIL(
                        io:format("Path  = ~p (~p)~n"
                                  "Short = ~p (~p)~n",
                                  [Path, Expect, S_Path, S_Expect]),
                        aggregate(
                          Classes,
                          case Expect of
                              false ->
                                  S_Expect =:= false andalso
                                  Path     =:= false andalso
                                  S_Path   =:= false;
                              _ ->
                                  conjunction(
                                    [{any, is_simple_path(R, V1, V2, Path)},
                                     {short,
                                      is_simple_path(R, V1, V2, S_Path) andalso
                                      length(S_Expect) =:= length(S_Path)}]
                                   )
                          end
                         )
                       )
                 end
                )
             )
       end
      ).

prop_get_cycle(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       begin
           R = list_digraph:from_list(L),
           ?FORALL(
              V, oneof(vertices(R)),
              ?WITH_G(
                 L,
                 begin
                     Expect   = gen_get_path_lists(R, V, V),
                     S_Expect = gen_get_short_path_lists(R, V, V),
                     Cycle    = get_cycle(G, V),
                     S_Cycle  = get_short_cycle(G, V),
                     Classes  = case Expect of
                                    false -> [false];
                                    _ -> [{Expect =:= Cycle, length(Expect)},
                                          {S_Expect =:= S_Cycle,
                                           length(S_Expect), length(Expect)}]
                                end,
                     ?WHENFAIL(
                        io:format("Cycle = ~p (~p)~n"
                                  "Short = ~p (~p)~n",
                                  [Cycle, Expect, S_Cycle, S_Expect]),
                        aggregate(
                          Classes,
                          case Expect of
                              false ->
                                  S_Expect =:= false andalso
                                  Cycle    =:= false andalso
                                  S_Cycle  =:= false;
                              _ ->
                                  conjunction(
                                    [{any, is_simple_path(R, V, V, Cycle)},
                                     {short,
                                      is_simple_path(R, V, V, S_Cycle) andalso
                                      length(S_Expect) =:= length(S_Cycle)}]
                                   )
                          end
                         )
                       )
                 end
                )
             )
       end
      ).

prop_reachable(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       begin
           R = list_digraph:from_list(L),
           Vs = vertices(R),
           ?FORALL(
              {Ss, V}, {list(oneof(Vs)), oneof(Vs)},
              ?WITH_G(
                 L,
                 begin
                     HasPath = lists:any(
                                 fun(X) -> get_path(G, X, V) =/= false end,
                                 Ss),
                     HasRevPath = lists:any(
                                 fun(X) -> get_path(G, V, X) =/= false end,
                                 Ss),
                     collect(
                       HasPath,
                       conjunction(
                         [{reachable,
                           equals(
                             HasPath orelse lists:member(V, Ss),
                             lists:member(V, reachable(G, Ss))
                            )},
                          {reachable_neighbours,
                           equals(
                             HasPath,
                             lists:member(V, reachable_neighbours(G, Ss))
                            )},
                          {reaching,
                           equals(
                             HasRevPath orelse lists:member(V, Ss),
                             lists:member(V, reaching(G, Ss))
                            )},
                          {reaching_neighbours,
                           equals(
                             HasRevPath,
                             lists:member(V, reaching_neighbours(G, Ss))
                            )}
                         ]
                        )
                      )
                 end
                )
             )
       end
      ).

prop_components(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       begin
           R  = list_digraph:from_list(L),
           UR = list_digraph:from_list(
                  [{V2, V1} || {V1, V2} <- L] ++ L),
           Vs = lists:sort(vertices(R)),
           ?FORALL(
              {V1, V2}, twoof(Vs),
              ?WITH_G(
                 L,
                 begin
                     Cs = components(G),
                     [C] = [X || X <- Cs, lists:member(V1, X)],
                     SCs = strong_components(G),
                     [SC] = [X || X <- SCs, lists:member(V1, X)],
                     Classes = case V1 =:= V2 of
                                   true  -> [same];
                                   false ->
                                       [{X, lists:member(V2, Y), length(Y)}
                                        || {X, Y} <- [{components, C},
                                                      {strong_components, SC}]]
                               end,
                     aggregate(
                       Classes,
                       conjunction(
                         [{components_vertices,
                           Vs =:= lists:sort(lists:append(Cs))},
                          {components_path,
                           V1 =:= V2 orelse
                           equals(
                             lists:member(V2, C),
                             get_path(UR, V1, V2) =/= false)},
                          {strong_components_vertices,
                           Vs =:= lists:sort(lists:append(SCs))},
                          {strong_components_path,
                           V1 =:= V2 orelse
                           equals(
                             lists:member(V2, SC),
                             get_path(R, V1, V2) =/= false andalso
                             get_path(R, V2, V1) =/= false)}
                         ])
                      )
                 end
                )
             )
       end
      ).

prop_preorder(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       begin
           R  = list_digraph:from_list(L),
           Vs = lists:sort(vertices(R)),
           ?WITH_G(
              L,
              begin
                  P = preorder(G),
                  conjunction(
                    [{vertices, equals(Vs, lists:sort(P))},
                     {order, is_preorder(R, P)}
                    ])
              end
             )
       end
      ).

is_preorder(G, P) ->
    is_preorder(G, vertices(G), P, #{}, []).

is_preorder(_, [], [], _, []) -> true;
is_preorder(G, [], P, Seen, [Possible|Stack]) ->
    is_preorder(G, not_seen(Seen, Possible, []), P, Seen, Stack);
is_preorder(G, Possible, [V|P], Seen, Stack) ->
    lists:member(V, Possible) andalso
    begin
        NewSeen = add2seen(V, Seen),
        Neighbours = not_seen(NewSeen, out_neighbours(G, V), []),
        NewStack = [Possible -- [V] | Stack],
        is_preorder(G, Neighbours, P, NewSeen, NewStack)
    end;
is_preorder(_, _, _, _, _) -> false.

prop_cyclic() ->
    Module = list_digraph,
    ?FORALL(
       L, non_empty(digraph()),
       ?WITH_G(
          L,
          begin
              Vs = vertices(G),
              HasCycle = fun(V) ->
                                 case get_cycle(G, V) of
                                     [V, V] -> false;
                                     C      -> C =/= false
                                 end
                         end,
              Cyclic = lists:any(HasCycle, Vs),
              equals(Cyclic, has_long_cycle(G))
          end
         )
      ).

prop_is_acyclic(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       ?WITH_G(
          L,
          begin
              Vs = vertices(G),
              HasCycle = fun(V) -> get_cycle(G, V) =/= false end,
              Cyclic = lists:any(HasCycle, Vs),
              equals(not Cyclic, is_acyclic(G))
          end
         )
      ).

prop_postorder(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       begin
           R  = list_digraph:from_list(L),
           Vs = lists:sort(vertices(R)),
           ?WITH_G(
              L,
              begin
                  P = postorder(G),
                  ?WHENFAIL(
                     io:format("~p~n", [P]),
                  conjunction(
                    [{vertices, equals(Vs, lists:sort(P))},
                     {order, is_postorder(R, P)}
                    ])
                    )
              end
             )
       end
      ).

is_postorder(G, P) ->
    is_postorder(G, lists:reverse(P), #{}).

is_postorder(_, [], _) -> true;
is_postorder(G, [V|P], Seen) ->
    SeenAndNoCycle = fun(X) ->
                             seen(X, Seen) andalso get_path(G, X, V) =:= false
                     end,
    not lists:any(SeenAndNoCycle, out_neighbours(G, V)) andalso
    is_postorder(G, P, add2seen(V, Seen)).

two_in_order([_,_|_] = L) ->
    ?LET(
       {A, B},
       ?LET(X, integer(1, length(L)-1), lists:split(X, L)),
       {oneof(A), hd(B)}
      ).

prop_topsort(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       begin
           R  = list_digraph:from_list(L),
           Vs = lists:sort(vertices(R)),
           ?WITH_G(
              L,
              case topsort(G) of
                  false ->
                      HasCycle = fun(V) ->
                                         case get_cycle(G, V) of
                                             [V, V] -> false;
                                             C      -> C =/= false
                                         end
                                 end,
                      lists:any(HasCycle, Vs);
                  [_] = P -> equals(Vs, P);
                  P ->
                      ?FORALL(
                         {V1, V2}, two_in_order(P),
                         conjunction(
                           [{all, equals(Vs, lists:sort(P))},
                            {order, equals(false, get_path(R, V2, V1))}])
                        )
              end
             )
       end
      ).

gen_properties_tests(Module) ->
    gen_properties_tests(Module, []).

gen_properties_tests(Module, Opts) ->
    [{atom_to_list(X), ?_assert(proper:quickcheck(Prop, Opts))}
     || X <- [  prop_list
              , prop_no_edges
              , prop_vertices
              , prop_no_vertices
              , prop_neighbours
              , prop_has_edge
              , prop_has_path
              , prop_get_path
              , prop_get_cycle
              , prop_reachable
              , prop_components
              , prop_preorder
              , prop_is_acyclic
              , prop_postorder
              , prop_topsort
             ],
        Prop <- [?MODULE:X(Module)]
    ].

gen_tests(Module) ->
    [{atom_to_list(X), Test}
     || X <- [test_empty_vertices],
        Test <- [?MODULE:X(Module)]
    ].

gen_has_path_test() ->
    R = list_digraph:from_list([{0, 0}, {0, 1}]),
    ?assertEqual(true, gen_has_path(R, [0, 1])).

gen_get_path_simple_path_test() ->
    R = list_digraph:from_list([{0, 0}, {0, 1}]),
    ?assertEqual([0,1], gen_get_path(R, 0, 1)).

is_preorder_test_() ->
    R = list_digraph:from_list([{0, 1}, {1, 2}]),
    [ ?_assert(    is_preorder(R, [0, 1, 2]))
    , ?_assert(not is_preorder(R, [0, 2, 1]))
    , ?_assert(not is_preorder(R, [1, 0, 2]))
    , ?_assert(    is_preorder(R, [1, 2, 0]))
    , ?_assert(    is_preorder(R, [2, 0, 1]))
    , ?_assert(    is_preorder(R, [2, 1, 0]))
    ].

is_postorder_test_() ->
    R = list_digraph:from_list([{0, 1}, {0, 2}]),
    [ ?_assert(not is_postorder(R, [0, 1, 2]))
    , ?_assert(not is_postorder(R, [0, 2, 1]))
    , ?_assert(not is_postorder(R, [1, 0, 2]))
    , ?_assert(    is_postorder(R, [1, 2, 0]))
    , ?_assert(not is_postorder(R, [2, 0, 1]))
    , ?_assert(    is_postorder(R, [2, 1, 0]))
    ].

has_long_cycle_test() ->
    ?assert(proper:quickcheck(prop_cyclic())).

-endif. %% TEST
