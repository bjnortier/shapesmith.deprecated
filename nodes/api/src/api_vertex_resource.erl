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

-module(api_vertex_resource).
-export([
         init/1, 
         allowed_methods/2,
         content_types_accepted/2,
         post_is_create/2,
         create_path/2,
         accept_content/2
        ]).
-include_lib("webmachine/include/webmachine.hrl").
-record(context, {method, json}).


init([]) -> 
    {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    Context1 = Context#context{ method=wrq:method(ReqData) },
    {['POST'], ReqData, Context1}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_content}], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}. 

create_path(ReqData, Context0) ->
    try 
        JSON = api_resource:parse_json(ReqData),
        Context1 = Context0#context{json=JSON},
        SHA = api_hash:hash_json(JSON),
        {SHA, ReqData, Context1}
    catch 
        invalid_json ->
            {{halt, 400}, ReqData, Context0}
    end.

accept_content(ReqData0, Context) ->
    try 
        JSON = api_resource:parse_json(ReqData0),
        ReqData1 = api_resource:json_response(JSON, ReqData0),
        {true, ReqData1, Context}
    catch 
        
        Err:Reason ->
            {{halt, 400}, ReqData0, Context}
    end.



