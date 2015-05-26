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

-export([ from_edgelist/1
        , to_edgelist/1
        , no_edges/1
        , vertices/1
        , no_vertices/1
        , in_neighbours/2
        , out_neighbours/2
        , sources/1
        , sinks/1
        , delete/1
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

from_edgelist(L) ->
    {_, D} = G = new(),
    [ case E of
          {V1, V2} ->
              digraph:add_vertex(D, V1),
              digraph:add_vertex(D, V2),
              digraph:add_edge(D, V1, V2);
          _ -> error(badarg)
      end
      || E <- L ],
    G.

to_edgelist({_, D}) ->
    [ case digraph:edge(D, E) of
          {_, V1, V2, _} -> {V1, V2}
      end
      || E <- digraph:edges(D) ].

no_edges({_, D}) -> digraph:no_edges(D).

vertices({_, D}) -> digraph:vertices(D).

no_vertices({_, D}) -> digraph:no_vertices(D).

in_neighbours({_, D}, V) -> digraph:in_neighbours(D, V).

out_neighbours({_, D}, V) -> digraph:out_neighbours(D, V).

sources(G) -> gen_digraph:gen_sources(G).

sinks(G) -> gen_digraph:gen_sinks(G).

delete({_, D}) -> digraph:delete(D).

%% -----------------------------------------------------------------------------
%% Tests
%% -----------------------------------------------------------------------------

-ifdef(TEST).

gen_properties_test_() ->
    gen_digraph:gen_properties_tests(?MODULE).

gen_tests_test_() ->
    gen_digraph:gen_tests(?MODULE).

-endif. %% TEST
