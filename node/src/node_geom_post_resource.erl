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

-module(node_geom_post_resource).
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
         malformed_request/2
        ]).


-include_lib("webmachine/include/webmachine.hrl").

-record(context, {id, geom_json}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['POST'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {false, ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_content}], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}. 

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    {"", ReqData, Context}. 

accept_content(ReqData, Context) ->
    {true, wrq:set_resp_header("Content-type", "application/json", ReqData), Context}.

malformed_request(ReqData, Context) ->
    Method = wrq:method(ReqData),
    malformed_request(ReqData, Context, Method).
    

malformed_request(ReqData, Context, 'POST') ->
    User = wrq:path_info(user, ReqData),
    Model = wrq:path_info(model, ReqData),
    case valid_json(wrq:req_body(ReqData)) of
	{true, JSON} ->
	    case node_master:create_geom(JSON) of
		{ok, Sha} ->
		    Path = io_lib:format("/~s/~s/geom/~s", [User, Model, Sha]),
		    ReqData1 = wrq:set_resp_body(
				 jiffy:encode({[{<<"path">>, iolist_to_binary(Path)}]}), ReqData),
		    {false, ReqData1, Context};
		{error, Reason = {validation, _}} ->
		    ReqData1 = error_response(Reason, ReqData),
		    {true, ReqData1, Context};
		Error = {error, _Reason} ->
		    node_log:error("~p~n", [Error]),
		    ReqData1 = error_response(Error, ReqData),
		    {{halt, 500}, ReqData1, Context}

	    end;
	false ->
	    ReqData1 = wrq:set_resp_body(<<"\"invalid json\"">>, ReqData),
	    {true, ReqData1, Context}
    end.

error_response({validation, ErrorParams}, ReqData) ->
    wrq:set_resp_body(
      jiffy:encode(
	{[{<<"validation">>, ErrorParams}]}), 
      ReqData);
error_response({error,worker_timeout}, ReqData) ->
    wrq:set_resp_body(
      jiffy:encode(
	{[{<<"error">>, <<"timeout">>}]}), 
      ReqData);
error_response(_, ReqData) ->
    wrq:set_resp_body(
      jiffy:encode(
	{[{<<"error">>, <<"internal error">>}]}), 
      ReqData).


valid_json(Body) ->
    try 
	{true, jiffy:decode(Body)}
    catch
	_:_ ->
            node_log:info("invalid JSON: ~p", [Body]),
	    false
    end.





