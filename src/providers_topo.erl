%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%%% Copyright 2012 Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%-------------------------------------------------------------------
%%% @author Joe Armstrong
%%% @author Eric Merritt
%%% @doc
%%%  This is a pretty simple topological sort for erlang. It was
%%%  originally written for ermake by Joe Armstrong back in '98. It
%%%  has been pretty heavily modified by Eric Merritt since '06 and
%%%  modified again for relx/rebar3 by Tristan Sloughter.
%%%
%%%  A partial order on the set S is a set of pairs {Xi,Xj} such that
%%%  some relation between Xi and Xj is obeyed.
%%%
%%%  A topological sort of a partial order is a sequence of elements
%%%  [X1, X2, X3 ...] such that if whenever {Xi, Xj} is in the partial
%%%  order i &lt; j
%%% @end
%%%-------------------------------------------------------------------
-module(providers_topo).

-export([sort/1]).

%%====================================================================
%% Types
%%====================================================================
-type pair() :: {DependentApp::atom(), PrimaryApp::atom()}.
-type name() :: AppName::atom().
-type element() :: name() | pair().

%%====================================================================
%% API
%%====================================================================

%% @doc Do a topological sort on the list of pairs.
-spec sort([pair()]) -> {ok, [atom()]} | {error, any()}.
sort(Pairs) ->
    iterate(Pairs, [], all(Pairs)).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Iterate over the system.  @private
-spec iterate([pair()], [name()], [name()]) ->
    {ok, [name()]} | {error, string()}.
iterate([], L, All) ->
    {ok, remove_duplicates(L ++ subtract(All, L))};
iterate(Pairs, L, All) ->
    case subtract(lhs(Pairs), rhs(Pairs)) of
        []  ->
            {error, "Cycle found in providers dependencies."};
        Lhs ->
            iterate(remove_pairs(Lhs, Pairs), L ++ Lhs, All)
    end.

-spec all([pair()]) -> [atom()].
all(L) ->
    lhs(L) ++ rhs(L).

-spec lhs([pair()]) -> [atom()].
lhs(L) ->
    [X || {X, _} <- L].

-spec rhs([pair()]) -> [atom()].
rhs(L) ->
    [Y || {_, Y} <- L].

%% @doc all the elements in L1 which are not in L2
%% @private
-spec subtract([element()], [element()]) -> [element()].
subtract(L1, L2) ->
    [X || X <- L1, not lists:member(X, L2)].

%% @doc remove dups from the list.  @private
-spec remove_duplicates([element()]) -> [element()].
remove_duplicates([H|T]) ->
  case lists:member(H, T) of
      true  ->
          remove_duplicates(T);
      false ->
          [H|remove_duplicates(T)]
  end;
remove_duplicates([]) ->
    [].

%% @doc
%%   removes all pairs from L2 where the first element
%%   of each pair is a member of L1
%%
%%   L2' L1 = [X] L2 = [{X,Y}].
%% @private
-spec remove_pairs([atom()], [pair()]) -> [pair()].
remove_pairs(L1, L2) ->
    [All || All={X, _Y} <- L2, not lists:member(X, L1)].

%%====================================================================
%% Tests
%%====================================================================
-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

topo_1_test() ->
    Pairs = [{one,two},{two,four},{four,six},
             {two,ten},{four,eight},
             {six,three},{one,three},
             {three,five},{five,eight},
             {seven,five},{seven,nine},
             {nine,four},{nine,ten}],
    ?assertMatch({ok, [one,seven,two,nine,four,six,three,five,eight,ten]},
                 sort(Pairs)).
topo_2_test() ->
    Pairs = [{app2, app1}, {zapp1, app1}, {stdlib, app1},
             {app3, app2}, {kernel, app1}, {kernel, app3},
             {app2, zapp1}, {app3, zapp1}, {zapp2, zapp1}],
    ?assertMatch({ok, [stdlib, kernel, zapp2,
                       app3, app2, zapp1, app1]},
                 sort(Pairs)).

topo_pairs_cycle_test() ->
    Pairs = [{app2, app1}, {app1, app2}, {stdlib, app1}],
    ?assertMatch({error, {_, {cycle, [{app2, app1}, {app1, app2}]}}},
                 sort(Pairs)).

-endif.
