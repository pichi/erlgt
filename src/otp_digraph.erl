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
%% @doc Example of wrapping OTP stdlib digraph
%%
%% The example module shows how to wrap existing digraph implementation for
%% using with ERLGT.
%%
%% @copyright 2015 Hynek Vychodil
%% @end
%%
%% -----------------------------------------------------------------------------
-module(otp_digraph).

-behaviour(gen_digraph).

-export([new/0]).

-export([ from_list/1
        , to_list/1
        , edges/1
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
        , is_edge/3
        , is_path/2
        , get_path/3
        , get_cycle/2
        , get_short_path/3
        , get_short_cycle/2
        , has_path/3
        , has_cycle/2
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
        , condensation/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

new() -> {?MODULE, digraph:new([private])}.

%% -----------------------------------------------------------------------------
%% Callbacks
%% -----------------------------------------------------------------------------

from_list(L) ->
    {_, D} = G = new(),
    [ case E of
          {V} -> digraph:add_vertex(D, V);
          {V1, V2} ->
              digraph:add_vertex(D, V1),
              digraph:add_vertex(D, V2),
              digraph:add_edge(D, V1, V2);
          _ -> error(badarg)
      end
      || E <- L ],
    G.

to_list(G) -> gen_digraph:gen_to_list(G).

edges({_, D}) ->
    [ case digraph:edge(D, E) of
          {_, V1, V2, _} -> {V1, V2}
      end
      || E <- digraph:edges(D) ].

no_edges({_, D}) -> digraph:no_edges(D).

vertices({_, D}) -> digraph:vertices(D).

no_vertices({_, D}) -> digraph:no_vertices(D).

in_neighbours({_, D}, V) -> digraph:in_neighbours(D, V).

out_neighbours({_, D}, V) -> digraph:out_neighbours(D, V).

in_degree({_, D}, V) -> digraph:in_degree(D, V).

out_degree({_, D}, V) -> digraph:out_degree(D, V).

sources(G) -> gen_digraph:gen_sources(G).

sinks(G) -> gen_digraph:gen_sinks(G).

delete({_, D}) -> digraph:delete(D).

is_edge(G, V1, V2) -> lists:member(V2, out_neighbours(G, V1)).

is_path(G, P) -> gen_digraph:gen_is_path(G, P).

get_path({_, D}, V1, V2) -> digraph:get_path(D, V1, V2).

get_cycle({_, D}, V) ->
    case digraph:get_cycle(D, V) of
        [V] = P -> [V|P];
        P -> P
    end.

get_short_path({_, D}, V1, V2) -> digraph:get_short_path(D, V1, V2).

get_short_cycle({_, D}, V) -> digraph:get_short_cycle(D, V).

has_path(G, V1, V2) -> gen_digraph:gen_has_path(G, V1, V2).

has_cycle(G, V) -> gen_digraph:gen_has_cycle(G, V).

reachable({_, D}, Vs) -> digraph_utils:reachable(Vs, D).

reachable_neighbours({_, D}, Vs) -> digraph_utils:reachable_neighbours(Vs, D).

reaching({_, D}, Vs) -> digraph_utils:reaching(Vs, D).

reaching_neighbours({_, D}, Vs) -> digraph_utils:reaching_neighbours(Vs, D).

components({_, D}) -> digraph_utils:components(D).

strong_components({_, D}) -> digraph_utils:strong_components(D).

preorder({_, D}) -> digraph_utils:preorder(D).

is_acyclic({_, D}) -> digraph_utils:is_acyclic(D).

postorder({_, D}) -> digraph_utils:postorder(D).

topsort({_, D}) -> digraph_utils:topsort(D).

condensation({Mod, D}) -> {Mod, digraph_utils:condensation(D)}.

%% -----------------------------------------------------------------------------
%% Tests
%% -----------------------------------------------------------------------------

-ifdef(TEST).

gen_properties_test_() ->
    gen_digraph:gen_properties_tests(?MODULE).

gen_tests_test_() ->
    gen_digraph:gen_tests(?MODULE).

-endif. %% TEST
