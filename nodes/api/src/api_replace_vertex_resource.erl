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

-module(api_replace_vertex_resource).
-export([
         init/1, 
         allowed_methods/2,
         content_types_accepted/2,
         post_is_create/2,
         create_path/2,
         accept_content/2,
         mutate_graph/3
        ]).
-include_lib("webmachine/include/webmachine.hrl").
-record(context, {method, user, design, graph_SHA, new_graph_SHA, mutate_result}).

init([]) -> 
    {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    Context1 = Context#context{ 
        method              = wrq:method(ReqData),
        user                = wrq:path_info(user, ReqData),
        design              = wrq:path_info(design, ReqData),
        graph_SHA           = wrq:path_info(graph_SHA, ReqData)
    },
    {['POST'], ReqData, Context1}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_content}], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}. 

create_path(ReqData, Context0 = #context{ user = User, 
                                          design = Design,
                                          graph_SHA = GraphSHA }) -> 
    try 
        {JSONProps} = api_resource:parse_json(ReqData),
        Graph0 = api_db:get(User, Design, graph, GraphSHA),
        {_, OriginalVertexSHA} = lists:keyfind(<<"original">>, 1, JSONProps),
        {_, ReplacementVertexSHA} = lists:keyfind(<<"replacement">>, 1, JSONProps),
        MutateResult = mutate_graph(OriginalVertexSHA, ReplacementVertexSHA, Graph0),
        {NewGraph, NotifyParents} = MutateResult,
        {ok, NewGraphSHA} = api_db:create(User, Design, graph, NewGraph),
        Context1 = Context0#context{ mutate_result = MutateResult, new_graph_SHA = NewGraphSHA },
        {NewGraphSHA, ReqData, Context1}
    catch 
        invalid_json ->
            {{halt, 400}, ReqData, Context0};
        _ ->
            {{halt, 400}, ReqData, Context0}
    end.

accept_content(ReqData0, Context = #context{ mutate_result = {_, NotifyParents},
                                             new_graph_SHA = NewGraphSHA }) ->
    Response = {[
        {<<"new_graph_sha">>, NewGraphSHA},
        {<<"notify_parents">>, NotifyParents}
    ]},
    ReqData1 = api_resource:json_response(Response, ReqData0),
    {true, ReqData1, Context}.

mutate_graph(OriginalVertexSHA, ReplacementVertexSHA, {GraphProps}) ->
    {_, Edges} = lists:keyfind(<<"edges">>, 1, GraphProps),
    {NewEdges, NotifyParents} = lists:foldr(
        fun({[{ParentVertexSHA, Children}]}, {EdgeAcc, NotifyParentsAcc}) -> 
                NewParentSHA = case ParentVertexSHA of
                    OriginalVertexSHA ->
                        ReplacementVertexSHA;
                    _ ->
                        ParentVertexSHA
                end,
                OriginalInChildren = lists:member(OriginalVertexSHA, Children),
                NotifyParentInChildren = lists:any(
                    fun(NotificationVertex) ->
                        lists:member(NotificationVertex, Children)
                    end,
                    NotifyParentsAcc),           
                NewNotifyParents = 
                    if
                        OriginalInChildren or NotifyParentInChildren ->
                            NotifyParentsAcc ++ [ParentVertexSHA];
                        true ->
                            NotifyParentsAcc
                    end,
                NewChildren = 
                    if
                        OriginalInChildren ->
                            replace_in_list(OriginalVertexSHA, ReplacementVertexSHA, Children);
                        true ->
                            Children
                    end,
                {[{[{NewParentSHA, NewChildren}]}|EdgeAcc], NewNotifyParents}
        end,
        {[], []},
        Edges),
    NewGraph = {lists:keyreplace(<<"edges">>, 1, GraphProps, {<<"edges">>, NewEdges})},
    {NewGraph, NotifyParents}.

replace_in_list(Old, New, List) ->
    lists:foldr(
        fun(X, Acc) when X =:= Old ->
            [New|Acc];
           (X, Acc) ->
            [X|Acc]
        end,
        [],
        List).

