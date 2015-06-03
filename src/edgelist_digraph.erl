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
-module(edgelist_digraph).

-behaviour(gen_digraph).

-export([new/0, from_list/1]).

-export([ from_edgelist/1
        , to_edgelist/1
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
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

new() -> {?MODULE, []}.

from_list(L) -> from_edgelist(L).

%% -----------------------------------------------------------------------------
%% Callbacks
%% -----------------------------------------------------------------------------

from_edgelist(L) ->
    [ ok || E <- L, case E of {_, _} -> true; _ -> error(badarg) end ],
    {?MODULE, lists:usort(L)}.

to_edgelist({_, L}) -> L.

no_edges(G) -> gen_digraph:gen_no_edges(G).

vertices(G) -> gen_digraph:gen_vertices(G).

no_vertices(G) -> gen_digraph:gen_no_vertices(G).

in_neighbours(G, V) -> gen_digraph:gen_in_neighbours(G, V).

out_neighbours(G, V) -> gen_digraph:gen_out_neighbours(G, V).

in_degree(G, V) -> gen_digraph:gen_in_degree(G, V).

out_degree(G, V) -> gen_digraph:gen_out_degree(G, V).

sources(G) -> gen_digraph:gen_sources(G).

sinks(G) -> gen_digraph:gen_sinks(G).

delete(_) -> true.

has_edge(G, V1, V2) -> gen_digraph:gen_has_edge(G, V1, V2).

has_path(G, P) -> gen_digraph:gen_has_path(G, P).

get_path(G, V1, V2) -> gen_digraph:gen_get_path(G, V1, V2).

get_cycle(G, V) -> gen_digraph:gen_get_cycle(G, V).

get_short_path(G, V1, V2) -> gen_digraph:gen_get_short_path(G, V1, V2).

%% -----------------------------------------------------------------------------
%% Tests
%% -----------------------------------------------------------------------------

-ifdef(TEST).

gen_properties_test_() ->
    gen_digraph:gen_properties_tests(?MODULE).

gen_tests_test_() ->
    gen_digraph:gen_tests(?MODULE).

-endif. %% TEST
