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

-module(node_signup_SUITE).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
	 validate_required,
	 validate_username,
	 validate_email,
	 validate_password,
	 signup_signin_signout
	].

init_per_suite(Config) ->
    ok = application:load(node),
    application:set_env(node, port, 8001),
    ok = application:set_env(node, db_module, node_mem_db),
    ok = application:set_env(node, auth_module, node_session_auth),
    ok = node:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(node),
    application:unload(node),
    ok.

init_per_testcase(_Testcase, Config) ->
    {ok, _} = node_mem_db:start_link(),
    Config.

end_per_testcase(_Testcase, _Config) ->
    node_mem_db:stop(),
    ok.

validate_required(_Config) ->
    
    PostBody = jiffy:encode({[]}),

    {ok,{{"HTTP/1.1",400,_}, ResponseHeaders, PostResponse}} = 
	httpc:request(post, {"http://localhost:8001/signup", [], "application/json", PostBody}, [], []),
    check_json_content_type(ResponseHeaders),
    {[{<<"username">>, <<"required">>},
      {<<"password1">>, <<"required">>},
      {<<"password2">>, <<"required">>}
     ]} 
	= jiffy:decode(iolist_to_binary(PostResponse)),
    
    ok.

validate_username(_Config) ->
    PostBody = jiffy:encode({[{<<"username">>, <<"&^%">>},
			      {<<"password1">>, <<"abc">>},
			      {<<"password2">>, <<"abc">>}
			     ]}),

    {ok,{{"HTTP/1.1",400,_}, ResponseHeaders, PostResponse}} = 
	httpc:request(post, {"http://localhost:8001/signup", [], "application/json", PostBody}, [], []),
    check_json_content_type(ResponseHeaders),
    {[{<<"username">>, <<"can only contain letters, numbers, dots, dashes and underscores">>}]} 
	= jiffy:decode(iolist_to_binary(PostResponse)),
    
    ok.

validate_email(_Config) ->
    InvalidAddresses = [
			<<"bjnortier">>,
			<<"@gmail.com">>,
			<<"gmail.com">>
		       ],
    lists:map(fun(InvalidAddress) ->
		      
		      {ok,{{"HTTP/1.1",400,_}, ResponseHeaders, PostResponse}} = 
			  httpc:request(post, {"http://localhost:8001/signup", [], "application/json", 
					       jiffy:encode({[{<<"username">>, <<"bjnortier">>}, {<<"password1">>, <<"x">>}, {<<"password2">>, <<"x">>}, {<<"emailAddress">>, InvalidAddress}
															   ]})}, [], []),
		      check_json_content_type(ResponseHeaders),
		      {[{<<"emailAddress">>, <<"invalid email address">>}]} 
			  = jiffy:decode(iolist_to_binary(PostResponse))
	      end,
	      InvalidAddresses).


validate_password(_Config) ->
    PostBody = jiffy:encode({[{<<"username">>, <<"bjnortier">>},
			      {<<"password1">>, <<"abc">>},
			      {<<"password2">>, <<"abe">>}
			     ]}),

    {ok,{{"HTTP/1.1",400,_}, ResponseHeaders, PostResponse}} = 
	httpc:request(post, {"http://localhost:8001/signup", [], "application/json", PostBody}, [], []),
    check_json_content_type(ResponseHeaders),
    {[{<<"password2">>, <<"doesn't match">>}]} 
	= jiffy:decode(iolist_to_binary(PostResponse)),
    
    ok.
    
signup_signin_signout(_Config) ->
    httpc:set_options([{cookies, enabled}]),
    PostBody = jiffy:encode({[{<<"username">>, <<"bjnortier">>},
			      {<<"password1">>, <<"abc">>},
			      {<<"password2">>, <<"abc">>}
			     ]}),

    {ok,{{"HTTP/1.1",201,_}, ResponseHeaders, PostResponse}} = 
	httpc:request(post, {"http://localhost.shapesmith.net:8001/signup", [], "application/json", PostBody}, [], []),
    check_json_content_type(ResponseHeaders),
    <<"created">> = jiffy:decode(iolist_to_binary(PostResponse)),
    {ok,{{"HTTP/1.1",409,_}, ResponseHeaders2, PostResponse2}} = 
	httpc:request(post, {"http://localhost.shapesmith.net:8001/signup", [], "application/json", PostBody}, [], []),
    check_json_content_type(ResponseHeaders2),
    {[{<<"username">>, <<"sorry - this username is already used">>}]} = jiffy:decode(iolist_to_binary(PostResponse2)),

    httpc:reset_cookies(),

    %% Failed signin
    FailedSigninPostBody = jiffy:encode({[{<<"username">>, <<"bjnortier">>},
					  {<<"password">>, <<"123">>}
					 ]}),
    {ok,{{"HTTP/1.1",403,_}, _, FailedSigninResponse}} = 
	httpc:request(post, {"http://localhost.shapesmith.net:8001/signin", [], "application/json", FailedSigninPostBody}, [], []),
    check_json_content_type(ResponseHeaders),
    {[{<<"password">>, <<"username/password combination is invalid">>}]} =
	jiffy:decode(iolist_to_binary(FailedSigninResponse)),

    {ok,{{"HTTP/1.1",302,_}, _, _}} = 
	httpc:request(get, {"http://localhost.shapesmith.net:8001/bjnortier/designs/", []}, 
		      [{autoredirect, false}], []),
    {ok,{{"HTTP/1.1",403,_}, _, _}} = 
	httpc:request(post, {"http://localhost:8001/bjnortier/iphonedock/", [], "application/json", "{}"}, [], []),
    {ok,{{"HTTP/1.1",403,_}, _, _}} = 
	httpc:request(get, {"http://localhost.shapesmith.net:8001/bjnortier/iphonedock", []}, 
		      [{autoredirect, false}], []),
    
    %% Successful signin
    SigninPostBody = jiffy:encode({[{<<"username">>, <<"bjnortier">>},
				    {<<"password">>, <<"abc">>}
				   ]}),
    {ok,{{"HTTP/1.1",201,_}, SigninResponseHeaders, SuccessfulSigninResponse}} = 
	httpc:request(post, {"http://localhost.shapesmith.net:8001/signin", [], "application/json", SigninPostBody}, [], []),
    check_json_content_type(ResponseHeaders),
    {_, SetCookie} = lists:keyfind("set-cookie", 1, SigninResponseHeaders),
    [SessionCookie, "Domain=.shapesmith.net"," Path=/"] = string:tokens(SetCookie, ";"),
    {[{<<"redirect">>, <<"/bjnortier/designs">>}]} = jiffy:decode(iolist_to_binary(SuccessfulSigninResponse)),
    
    {ok,{{"HTTP/1.1",200,_}, _, _}} = 
	httpc:request(get, {"http://localhost.shapesmith.net:8001/bjnortier/designs/", []}, [], []),
    {ok,{{"HTTP/1.1",200,_}, _, _}} = 
	httpc:request(post, {"http://localhost.shapesmith.net:8001/bjnortier/designA/", [], "application/json", "{}"}, [], []),
    {ok,{{"HTTP/1.1",200,_}, _, _}} = 
	httpc:request(get, {"http://localhost.shapesmith.net:8001/bjnortier/designA", []}, [], []),
    {ok,{{"HTTP/1.1",403,_}, _, _}} = 
	httpc:request(get, {"http://localhost.shapesmith.net:8001/foo/designB", []}, [{autoredirect, false}], []),

    %% Signout
    httpc:reset_cookies(),
    {ok,{{"HTTP/1.1",200,_}, _, _}} = 
	httpc:request(get, {"http://localhost.shapesmith.net:8001/bjnortier/designs/", [{"set-cookie", SessionCookie}]}, [], []),

    {ok,{{"HTTP/1.1",302,_}, SignoutHeaders, _}} = 
	httpc:request(get, {"http://localhost.shapesmith.net:8001/signout", []}, [{autoredirect, false}], []),
    {_, "http://localhost.shapesmith.net:8001"} = lists:keyfind("location", 1, SignoutHeaders),

    {ok,{{"HTTP/1.1",302,_}, _, _}} = 
	httpc:request(get, {"http://localhost.shapesmith.net:8001/bjnortier/designs/", [{"set-cookie", SessionCookie}]}, [{autoredirect, false}], []),


    ok.



check_json_content_type(Headers) ->
    {Headers, {_, "application/json"}} = {Headers, lists:keyfind("content-type", 1, Headers)}.
