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

-module(node_ref_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([
	 init/1, 
         allowed_methods/2,
	 content_types_provided/2,
	 provide_content/2,
         content_types_accepted/2,
         accept_content/2,
	 resource_exists/2,
         malformed_request/2
        ]).
-include_lib("webmachine/include/webmachine.hrl").

-record(context, {method, adapter, user, design, ref_type, ref, request_json}).

init([{adapter_mod, Adapter}]) -> {ok, #context{adapter = Adapter}}.

allowed_methods(ReqData, Context) -> 
    Context1 = Context#context{ method=wrq:method(ReqData),
				user=wrq:path_info(user, ReqData),
				design=wrq:path_info(design, ReqData), 
				ref_type=wrq:path_info(reftype, ReqData),
				ref=wrq:path_info(ref, ReqData) },
    {['GET', 'PUT'], ReqData, Context1}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_content}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_content}], ReqData, Context}.

malformed_request(ReqData, Context = #context{ method='GET'}) ->
    {false, ReqData, Context};
malformed_request(ReqData, Context = #context{ adapter=Adapter, 
					       user=User, 
					       design=Design,
					       ref_type=RefType,
					       ref=Ref,
					       method='PUT'}) ->
    Body = wrq:req_body(ReqData),
    try
	RequestJSON = jiffy:decode(Body),
	case Adapter:validate(User, Design, RefType, Ref, RequestJSON) of
	    ok ->
		{false, ReqData, Context#context{ request_json = RequestJSON }};
	    {error, ResponseJSON} ->
		{true, node_resource:json_response(ResponseJSON, ReqData), Context};
	    {error, Code, ResponseJSON} ->
		{{halt, Code}, node_resource:json_response(ResponseJSON, ReqData), Context}
	end

    catch
	_:_ ->
            lager:warning("invalid JSON: ~p", [Body]),
	    {true, node_resource:json_response(<<"invalid JSON">>, ReqData), Context}
    end.


resource_exists(ReqData, Context = #context{ adapter=Adapter, 
					     method=Method,
					     user=User, 
					     design=Design,
					     ref_type=RefType,
					     ref=Ref }) ->

    Exists = Adapter:exists(User, Design, RefType, Ref),
    case {Method, Exists} of
	{'GET', false} ->
	    {false, node_resource:json_response(<<"not found">>, ReqData), Context};
	_ ->
	    {Exists, ReqData, Context}
    end.

accept_content(ReqData, Context = #context{ adapter=Adapter, 
					  user=User, 
					  design=Design,
					  ref_type=RefType,
					  ref=Ref,
					  request_json=RequestJSON }) ->

    case Adapter:update(User, Design, RefType, Ref, RequestJSON) of
	{ok, ResponseJSON} ->
	    {true, node_resource:json_response(ResponseJSON, ReqData), Context};
	{error, ResponseJSON} ->
	    {false, node_resource:json_response(ResponseJSON, ReqData), Context};
	{error, Code, ResponseJSON} ->
	    {{halt, Code}, node_resource:json_response(ResponseJSON, ReqData), Context}
    end.


provide_content(ReqData,  Context = #context{ adapter=Adapter, 
					      user=User, 
					      design=Design,
					      ref_type=RefType,
					      ref=Ref }) ->
    {jiffy:encode(Adapter:get(User, Design, RefType, Ref)), ReqData, Context}.

