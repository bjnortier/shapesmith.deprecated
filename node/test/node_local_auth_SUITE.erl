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

-module(node_local_auth_SUITE).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
	 redirect_signin_and_signup
	].

init_per_suite(Config) ->
    ok = application:load(node),
    application:set_env(node, port, 8001),
    ok = application:set_env(node, auth_module, node_local_auth),
    ok = node:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(node),
    application:unload(node),
    ok.

redirect_signin_and_signup(_Config) ->
    {ok,{{"HTTP/1.1",302,_}, Headers, _Response}} = 
	httpc:request(get, {"http://localhost:8001/signin", []}, [{autoredirect, false}], []),
    {_, "http://localhost:8001/local/designs/"} = lists:keyfind("location", 1, Headers),
    
    {ok,{{"HTTP/1.1",302,_}, Headers, _Response}} = 
	httpc:request(get, {"http://localhost:8001/signup", []}, [{autoredirect, false}], []),
    {_, "http://localhost:8001/local/designs/"} = lists:keyfind("location", 1, Headers).

