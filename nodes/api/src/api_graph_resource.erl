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

-module(api_graph_resource).
-export([
         init/1, 
         allowed_methods/2,
         content_types_accepted/2,
         post_is_create/2,
         create_path/2,
         accept_content/2,
         content_types_provided/2,
         resource_exists/2,
         provide_content/2
        ]).
-include_lib("webmachine/include/webmachine.hrl").
-record(context, {method, user, design, json}).


init([]) -> 
    {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    Context1 = Context#context{ 
        method=wrq:method(ReqData),
        user=wrq:path_info(user, ReqData),
        design=wrq:path_info(design, ReqData) 
    },
    {['POST', 'GET'], ReqData, Context1}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_content}], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}. 

create_path(ReqData, Context0) ->
    % try 
        JSON = api_resource:parse_json(ReqData),
        Context1 = Context0#context{json=JSON},
        SHA = api_hash:hash_json(JSON),
        {SHA, ReqData, Context1}.
    % catch 
    %     invalid_json ->
    %         {{halt, 400}, ReqData, Context0}
    % end.

accept_content(ReqData0, Context = #context{ user = User, 
                                             design = Design }) ->
    JSON = api_resource:parse_json(ReqData0),
    {ok, SHA} = api_db:create(User, Design, graph, JSON),
    ReqData1 = api_resource:json_response(list_to_binary(SHA), ReqData0),
    {true, ReqData1, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_content}], ReqData, Context}.

resource_exists(ReqData, Context = #context{ method=Method,
                                             user=User, 
                                             design=Design }) ->
    case {Method, wrq:path_info(sha, ReqData)} of 
        {'POST', undefined} ->
            {true, ReqData, Context};
        {'GET', undefined} ->
            {false, ReqData, Context};
        {_, SHA} ->
         
            {api_db:exists(User, Design, graph, SHA), ReqData, Context}
    end.

provide_content(ReqData, Context = #context{ user=User, 
                                             design=Design }) ->
    SHA = wrq:path_info(sha, ReqData),
    JSON = api_db:get(User, Design, graph, SHA),
    {jiffy:encode(JSON), ReqData, Context}.

