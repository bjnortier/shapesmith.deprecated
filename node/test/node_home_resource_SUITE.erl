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

-module(node_home_resource_SUITE).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
	 redirect_to_local_designs,
	 redirect_to_session_designs
	].

init_per_suite(Config) ->
    ok = application:load(node),
    application:set_env(node, port, 8001),
    ok = application:set_env(node, db_module, node_mem_db),
    {ok, _} = node_mem_db:start(),
    Config.

end_per_suite(_Config) ->
    node_mem_db:stop(),
    application:unload(node),
    ok.

init_per_testcase(redirect_to_session_designs, Config) ->
    ok = application:set_env(node, auth_module, node_session_auth),
    ok = node:start(),
    Config;
init_per_testcase(redirect_to_local_designs, Config) ->
    ok = application:set_env(node, auth_module, node_local_auth),
    ok = node:start(),
    Config.

end_per_testcase(_Testcase, _Config) ->
    ok = application:stop(node),
    ok.

redirect_to_local_designs(_Config) ->
    {ok,{{"HTTP/1.1",302,_}, Headers, _Response}} = 
	httpc:request(get, {"http://localhost:8001/", []}, [{autoredirect, false}], []),
    {_, "http://localhost:8001/local/designs/"} = lists:keyfind("location", 1, Headers).

redirect_to_session_designs(_Config) ->
    PostBody = jiffy:encode({[{<<"username">>, <<"bjnortier">>},
			      {<<"password1">>, <<"abc">>},
			      {<<"password2">>, <<"abc">>}
			     ]}),

    httpc:set_options([{cookies, enabled}]),
    {ok,{{"HTTP/1.1",201,_}, _, _}} = 
	httpc:request(post, {"http://localhost.shapesmith.net:8001/signup", [], "application/json", PostBody}, [], []),

    {ok,{{"HTTP/1.1",302,_}, Headers, _Response}} = 
	httpc:request(get, {"http://localhost.shapesmith.net:8001/", []}, [{autoredirect, false}], []),
    {_, "http://localhost.shapesmith.net:8001/bjnortier/designs/"} = lists:keyfind("location", 1, Headers).

