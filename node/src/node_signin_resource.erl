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

-module(node_signin_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([
	 init/1, 
         allowed_methods/2,
	 content_types_provided/2,
	 resource_exists/2,
	 provide_content/2,
	 content_types_accepted/2,
	 post_is_create/2,
	 create_path/2,
	 accept_content/2,
	 malformed_request/2
        ]).
-include_lib("webmachine/include/webmachine.hrl").
-record(context, {method, email_address}).

init([]) -> 
    {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    Context1 = Context#context{ method=wrq:method(ReqData) },
    {['GET', 'POST'], ReqData, Context1}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", provide_content}], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {true, ReqData, Context}.

provide_content(ReqData, Context) ->
    WalrusContext = [{fields, [
			       [{name, "username"}, {placeholder, "username"}, {type, "text"}],
			       [{name, "password"}, {placeholder, "password"}, {type, "password"}]
			      ]}],
    Rendered = node_walrus:render_template(node_views_signin, WalrusContext),
    {Rendered, ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_content}], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    {"not used", ReqData, Context}. 

malformed_request(ReqData, Context = #context{ method='GET'}) ->
    {false, ReqData, Context};
malformed_request(ReqData, Context = #context{ method='POST'}) ->
    Body = wrq:req_body(ReqData),
    try
	JSON = jiffy:decode(Body),
	case validate([fun validate_required/1], JSON) of
	    ok ->
		{false, ReqData, Context};
	    {error, Response} ->
		{true, node_resource:json_response(Response, ReqData), Context}
	end
	    
    catch
	throw:{error,{_Pos,invalid_json}} ->
            lager:warning("invalid JSON: ~p", [Body]),
	    {true, node_resource:json_response(<<"invalid JSON">>, ReqData), Context};
	Err:Reason ->
	    lager:warning("Unexpected exception during validate: ~p:~p", [Err, Reason]),
	    {true,  node_resource:json_response(<<"internal error">>, ReqData), Context}
    end.


accept_content(ReqData, Context) ->
    Body = wrq:req_body(ReqData),
    {Props} = jiffy:decode(Body),
    {_, Username} = lists:keyfind(<<"username">>, 1, Props),
    {_, Password} = lists:keyfind(<<"password">>, 1, Props),
    case node_db:validate_password(binary_to_list(Username), 
				   binary_to_list(Password)) of
	ok ->
	    Redirect = "/" ++ binary_to_list(Username) ++ "/designs",
	    {true, node_resource:json_response({[{<<"redirect">>, list_to_binary(Redirect)}]}, ReqData), Context};
	_ ->
	    Response = {[{<<"password">>, <<"username/password combination is invalid">>}]},
	    {{halt, 403}, node_resource:json_response(Response, ReqData), Context}
		
    end.

validate([Hd|Rest], JSON) ->
    case Hd(JSON) of
	{error, Reason} ->
	    {error, Reason};
	ok ->
	    validate(Rest, JSON)
    end;
validate([], _JSON) ->
    ok.

validate_required({Props}) ->
    RequiredFields = [<<"username">>, <<"password">>],
    Missing = lists:foldr(fun(Field, Acc) ->
				  case lists:keyfind(Field, 1, Props) of
				      {_, Value} ->
					  case string:strip(binary_to_list(Value)) of
					      "" ->
						  [{Field, <<"required">>}|Acc];
					      _ ->
						  Acc
					  end;
				      false ->
					  [{Field, <<"required">>}|Acc]
				  end
			  end,
			  [],
			  RequiredFields),
    case Missing of 
	[] ->
	    ok;
	MissingProps ->
	    {error, {MissingProps}}
    end.
