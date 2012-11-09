%% -*- mode: erlang -*-
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% Copyright 2011 Benjamin Nortier
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.

-module(api_replace_vertex_resource_tests).
-author('Benjamin Nortier <bjnortier@shapesmith.net>').
-include_lib("eunit/include/eunit.hrl").

no_recursion_test_() ->
    OriginalGraph = {[{<<"edges">>, [
        {[{<<"aa">>, []}]},
        {[{<<"bb">>, []}]}
    ]}]},
    {ResultGraph, NotifyParents} = api_replace_vertex_resource:mutate_graph(<<"bb">>, <<"cc">>, OriginalGraph),
    ExpectedGraph = {[{<<"edges">>, [
        {[{<<"aa">>, []}]},
        {[{<<"cc">>, []}]}
    ]}]},
    [
     ?_assertEqual(ExpectedGraph, ResultGraph),
     ?_assertEqual([], NotifyParents)
    ].

simple_dag_test_() ->
    OriginalGraph = {[{<<"edges">>, [
        {[{<<"aa">>, [<<"bb">>]}]},
        {[{<<"bb">>, []}]}
    ]}]},
    {ResultGraph, NotifyParents} = api_replace_vertex_resource:mutate_graph(<<"bb">>, <<"cc">>, OriginalGraph),
    ExpectedGraph = {[{<<"edges">>, [
        {[{<<"aa">>, [<<"cc">>]}]},
        {[{<<"cc">>, []}]}
    ]}]},
    [
     ?_assertEqual(ExpectedGraph, ResultGraph),
     ?_assertEqual([<<"aa">>], NotifyParents)
    ].

shared_leaf_test_() ->
    OriginalGraph = {[{<<"edges">>, [
        {[{<<"aa">>, [<<"cc">>]}]},
        {[{<<"bb">>, [<<"cc">>]}]},
        {[{<<"cc">>, []}]}
    ]}]},
    {ResultGraph, NotifyParents} = api_replace_vertex_resource:mutate_graph(<<"cc">>, <<"dd">>, OriginalGraph),
    ExpectedGraph = {[{<<"edges">>, [
        {[{<<"aa">>, [<<"dd">>]}]},
        {[{<<"bb">>, [<<"dd">>]}]},
        {[{<<"dd">>, []}]}
    ]}]},
    [
     ?_assertEqual(ExpectedGraph, ResultGraph),
     ?_assertEqual([<<"bb">>, <<"aa">>], NotifyParents)
    ].

shared_intermediate_vertex_test_() ->
    OriginalGraph = {[{<<"edges">>, [
        {[{<<"aa">>, [<<"cc">>]}]},
        {[{<<"bb">>, [<<"cc">>]}]},
        {[{<<"cc">>, [<<"dd">>]}]},
        {[{<<"cc">>, [<<"ee">>]}]},
        {[{<<"dd">>, []}]},
        {[{<<"ee">>, []}]}
    ]}]},
    {ResultGraph, NotifyParents} = api_replace_vertex_resource:mutate_graph(<<"dd">>, <<"dd2">>, OriginalGraph),
    ExpectedGraph = {[{<<"edges">>, [
        {[{<<"aa">>, [<<"cc">>]}]},
        {[{<<"bb">>, [<<"cc">>]}]},
        {[{<<"cc">>, [<<"dd2">>]}]},
        {[{<<"cc">>, [<<"ee">>]}]},
        {[{<<"dd2">>, []}]},
        {[{<<"ee">>, []}]}
    ]}]},
    [
     ?_assertEqual(ExpectedGraph, ResultGraph),
     ?_assertEqual([<<"cc">>, <<"bb">>, <<"aa">>], NotifyParents)
    ].

potential_duplicate_notifications_test_() ->
    OriginalGraph = {[{<<"edges">>, [
        {[{<<"aa">>, [<<"bb">>, <<"cc">>]}]},
        {[{<<"bb">>, [<<"dd">>]}]},
        {[{<<"cc">>, [<<"dd">>]}]},
        {[{<<"dd">>, []}]}
    ]}]},
    {ResultGraph, NotifyParents} = api_replace_vertex_resource:mutate_graph(<<"dd">>, <<"dd2">>, OriginalGraph),
    ExpectedGraph = {[{<<"edges">>, [
        {[{<<"aa">>, [<<"bb">>, <<"cc">>]}]},
        {[{<<"bb">>, [<<"dd2">>]}]},
        {[{<<"cc">>, [<<"dd2">>]}]},
        {[{<<"dd2">>, []}]}
    ]}]},
    [
     ?_assertEqual(ExpectedGraph, ResultGraph),
     ?_assertEqual([<<"cc">>, <<"bb">>, <<"aa">>], NotifyParents)
    ].

