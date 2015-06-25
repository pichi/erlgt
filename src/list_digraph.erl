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
%% @doc Example digraph implementation as list of edges
%%
%% The example module shows how to implement custom digraph implementation.
%% It should be simplest possible digraph module.
%%
%% @copyright 2015 Hynek Vychodil
%% @end
%%
%% -----------------------------------------------------------------------------
-module(list_digraph).

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

new() -> {?MODULE, {[], []}}.

%% -----------------------------------------------------------------------------
%% Callbacks
%% -----------------------------------------------------------------------------

from_list(L) ->
    Vs = [ V || E <- L, V <- case E of
                                 {V} -> [V];
                                 {V1, V2} -> [V1, V2];
                                 _ -> error(badarg)
                             end ],
    Es = [ E || {_, _} = E <- L ],
    {?MODULE, {lists:usort(Vs), lists:sort(Es)}}.

to_list(G) -> gen_digraph:gen_to_list(G).

edges({_, {_, Es}}) -> Es.

no_edges(G) -> gen_digraph:gen_no_edges(G).

vertices({_, {Vs, _}}) -> Vs.

no_vertices(G) -> gen_digraph:gen_no_vertices(G).

in_neighbours(G, V) -> gen_digraph:gen_in_neighbours(G, V).

out_neighbours(G, V) -> gen_digraph:gen_out_neighbours(G, V).

in_degree(G, V) -> gen_digraph:gen_in_degree(G, V).

out_degree(G, V) -> gen_digraph:gen_out_degree(G, V).

sources(G) -> gen_digraph:gen_sources(G).

sinks(G) -> gen_digraph:gen_sinks(G).

delete(_) -> true.

is_edge(G, V1, V2) -> gen_digraph:gen_is_edge(G, V1, V2).

is_path(G, P) -> gen_digraph:gen_is_path(G, P).

get_path(G, V1, V2) -> gen_digraph:gen_get_path(G, V1, V2).

get_cycle(G, V) -> gen_digraph:gen_get_cycle(G, V).

get_short_path(G, V1, V2) -> gen_digraph:gen_get_short_path(G, V1, V2).

get_short_cycle(G, V) -> gen_digraph:gen_get_short_cycle(G, V).

reachable(G, Vs) -> gen_digraph:gen_reachable(G, Vs).

reachable_neighbours(G, Vs) -> gen_digraph:gen_reachable_neighbours(G, Vs).

reaching(G, Vs) -> gen_digraph:gen_reaching(G, Vs).

reaching_neighbours(G, Vs) -> gen_digraph:gen_reaching_neighbours(G, Vs).

components(G) -> gen_digraph:gen_components(G).

strong_components(G) -> gen_digraph:gen_strong_components(G).

preorder(G) -> gen_digraph:gen_preorder(G).

is_acyclic(G) -> gen_digraph:gen_is_acyclic(G).

postorder(G) -> gen_digraph:gen_postorder(G).

topsort(G) -> gen_digraph:gen_topsort(G).

%% -----------------------------------------------------------------------------
%% Tests
%% -----------------------------------------------------------------------------

-ifdef(TEST).

gen_properties_test_() ->
    gen_digraph:gen_properties_tests(?MODULE).

gen_tests_test_() ->
    gen_digraph:gen_tests(?MODULE).

-endif. %% TEST
