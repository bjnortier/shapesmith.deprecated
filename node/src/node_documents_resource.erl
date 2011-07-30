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

-module(node_documents_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([
         init/1, 
         allowed_methods/2,
	 post_is_create/2,
         process_post/2,
	 resource_exists/2
        ]).


-include_lib("webmachine/include/webmachine.hrl").

-record(context, {}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['POST'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    case wrq:method(ReqData) of
        'POST' ->
            {true, ReqData, Context};
        'GET' ->
            {true, ReqData, Context}
    end.

post_is_create(ReqData, Context) ->
    {false, ReqData, Context}.

process_post(ReqData, Context) ->
    {ok, DB} = application:get_env(node, db_module),
    Id = DB:create(doc, []),
    ReqData1 = wrq:set_resp_body("{\"path\": \"/doc/" ++ Id ++ "\"}", ReqData),
    {true, ReqData1, Context}.


