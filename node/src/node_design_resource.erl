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

-module(node_design_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([
         init/1, 
         allowed_methods/2,
         content_types_accepted/2,
         accept_content/2,
	 post_is_create/2,
	 allow_missing_post/2,
	 create_path/2,
	 resource_exists/2,
         malformed_request/2,
	 content_types_provided/2,
	 provide_content/2
        ]).
-include_lib("webmachine/include/webmachine.hrl").
-record(context, {adapter, user, design, exists, request_json}).

init([{adapter_mod, Adapter}]) -> {ok, #context{adapter = Adapter}}.

allowed_methods(ReqData, Context) -> 
    ReqData1 = wrq:set_resp_header("Content-type", "application/json", ReqData),
    {['GET', 'POST'], ReqData1, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_content}], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}. 

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    {"not used", ReqData, Context}. 

accept_content(ReqData, Context) ->
    Adapter = Context#context.adapter,
    User = wrq:path_info(user, ReqData),
    Design = wrq:path_info(design, ReqData),
    case Adapter:create(User, Design, ReqData) of
	{ok, ResponseJSON} ->
	    {true, wrq:set_resp_body(ResponseJSON, ReqData), Context};
	{error, ResponseJSON} ->
	    {false, wrq:set_resp_body(ResponseJSON, ReqData), Context};
	{error, Code, ResponseJSON} ->
	    {{halt, Code}, wrq:set_resp_body(ResponseJSON, ReqData), Context}
    end.

malformed_request(ReqData, Context) ->
    Method = wrq:method(ReqData),
    User = wrq:path_info(user, ReqData),
    Design = wrq:path_info(design, ReqData),
    Adapter = Context#context.adapter,
    Exists = Adapter:exists(User, Design),
    Context1 = Context#context{ exists=Exists, user=User, design=Design },
    malformed_request(ReqData, Context1, Method).

malformed_request(ReqData, Context, 'GET') ->
    {false, ReqData, Context};
malformed_request(ReqData, Context = #context{ exists = true }, 'POST') ->
    {true, wrq:set_resp_body(<<"\"already exists\"">>, ReqData), Context};
malformed_request(ReqData, Context = #context{ exists = false }, 'POST') ->
    Body = wrq:req_body(ReqData),
    try 
	RequestJSON = jiffy:decode(Body),
	User = wrq:path_info(user, ReqData),
	Design = wrq:path_info(design, ReqData),
	Adapter = Context#context.adapter,
	case Adapter:validate(User, Design, RequestJSON) of
	    ok ->
		{false, ReqData, Context#context{ request_json = RequestJSON }};
	    {error, ResponseJSON} ->
		{true, wrq:set_resp_body(ResponseJSON, ReqData), Context};
	    {error, Code, ResponseJSON} ->
		{{halt, Code}, wrq:set_resp_body(ResponseJSON, ReqData), Context}
	end

    catch
	throw:{error,{1,invalid_json}} ->
            lager:warning("invalid JSON: ~p", [Body]),
	    {true, wrq:set_resp_body(<<"\"invalid JSON\"">>, ReqData), Context};
	Err:Reason ->
	    lager:warning("Unexpected exception: ~p:~p", [Err, Reason]),
	    {true,  wrq:set_resp_body(<<"\"internal error\"">>, ReqData), Context}
    end.

resource_exists(ReqData, Context) ->
    Method = wrq:method(ReqData),
    case {Method, Context#context.exists} of
	{'GET', false} ->
	    {false, wrq:set_resp_body(<<"\"not found\"">>, ReqData), Context};
	_ ->
	    {Context#context.exists, ReqData, Context}
    end.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    Adapter = Context#context.adapter,
    Geometry = Adapter:get(Context#context.user, Context#context.design),
    {Geometry, ReqData, Context}.
