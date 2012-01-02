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
	 create_user
	].

init_per_suite(Config) ->
    ok = application:load(node),
    application:set_env(node, port, 8001),
    application:start(inets),
    application:start(lager),
    ok = application:set_env(node, db_module, node_mem_db),
    ok = node:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(node),
    application:unload(node),
    application:stop(lager),
    application:stop(inets),
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
	httpc:request(post, {"http://localhost:8001/signup_email", [], "application/json", PostBody}, [], []),
    check_json_content_type(ResponseHeaders),
    {[{<<"username">>, <<"required">>},
      {<<"password-1">>, <<"required">>},
      {<<"password-2">>, <<"required">>}
     ]} 
	= jiffy:decode(iolist_to_binary(PostResponse)),
    
    ok.

validate_username(_Config) ->
    PostBody = jiffy:encode({[{<<"username">>, <<"&^%">>},
			      {<<"password-1">>, <<"abc">>},
			      {<<"password-2">>, <<"abc">>}
			     ]}),

    {ok,{{"HTTP/1.1",400,_}, ResponseHeaders, PostResponse}} = 
	httpc:request(post, {"http://localhost:8001/signup_email", [], "application/json", PostBody}, [], []),
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
			  httpc:request(post, {"http://localhost:8001/signup_email", [], "application/json", 
					       jiffy:encode({[{<<"username">>, <<"bjnortier">>}, {<<"password-1">>, <<"x">>}, {<<"password-2">>, <<"x">>}, {<<"email-address">>, InvalidAddress}
															   ]})}, [], []),
		      check_json_content_type(ResponseHeaders),
		      {[{<<"email-address">>, <<"invalid email address">>}]} 
			  = jiffy:decode(iolist_to_binary(PostResponse))
	      end,
	      InvalidAddresses).


validate_password(_Config) ->
    PostBody = jiffy:encode({[{<<"username">>, <<"bjnortier">>},
			      {<<"password-1">>, <<"abc">>},
			      {<<"password-2">>, <<"abe">>}
			     ]}),

    {ok,{{"HTTP/1.1",400,_}, ResponseHeaders, PostResponse}} = 
	httpc:request(post, {"http://localhost:8001/signup_email", [], "application/json", PostBody}, [], []),
    check_json_content_type(ResponseHeaders),
    {[{<<"password-2">>, <<"doesn't match">>}]} 
	= jiffy:decode(iolist_to_binary(PostResponse)),
    
    ok.
    
create_user(_Config) ->
    PostBody = jiffy:encode({[{<<"username">>, <<"bjnortier">>},
			      {<<"password-1">>, <<"abc">>},
			      {<<"password-2">>, <<"abc">>}
			     ]}),

    {ok,{{"HTTP/1.1",200,_}, ResponseHeaders, PostResponse}} = 
	httpc:request(post, {"http://localhost:8001/signup_email", [], "application/json", PostBody}, [], []),
    check_json_content_type(ResponseHeaders),
    <<"created">> = jiffy:decode(iolist_to_binary(PostResponse)),
    
    {ok,{{"HTTP/1.1",409,_}, ResponseHeaders2, PostResponse2}} = 
	httpc:request(post, {"http://localhost:8001/signup_email", [], "application/json", PostBody}, [], []),
    check_json_content_type(ResponseHeaders2),
    <<"already exists">> = jiffy:decode(iolist_to_binary(PostResponse2)).

check_json_content_type(Headers) ->
    {Headers, {_, "application/json"}} = {Headers, lists:keyfind("content-type", 1, Headers)}.
