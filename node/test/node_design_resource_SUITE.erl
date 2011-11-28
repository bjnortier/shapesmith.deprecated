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

-module(node_design_resource_SUITE).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
	 malformed_create,
	 not_found,
	 create_new,
	 already_exists,
	 save
	].

init_per_suite(Config) ->
    ok = application:load(node),
    application:set_env(node, port, 8001),
    application:start(inets),
    ok = application:set_env(node, db_module, node_mem_db),
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

malformed_create(_Config) ->
    {ok,{{"HTTP/1.1",400,_}, _, PostResponse1}} = 
	httpc:request(put, {"http://localhost:8001/bjnortier/iphonedock/", [], "application/json", "[]"}, [], []),
    "\"only {} accepted\"" = PostResponse1,

    {ok,{{"HTTP/1.1",400,_}, _, PostResponse2}} = 
	httpc:request(put, {"http://localhost:8001/bjnortier/iphonedock/", [], "application/json", "@$&^%£^"}, [], []),
    "\"invalid JSON\"" = PostResponse2.


not_found(_Config) ->
    {ok,{{"HTTP/1.1",404,_}, _, GetResponse}} = 
	httpc:request(get, {"http://localhost:8001/bjnortier/undefined/", []}, [], []),
    "\"not found\"" = GetResponse.

create_new(_Config) ->
    %% Create
    {ok,{{"HTTP/1.1",200,_}, _, PostResponse}} = 
	httpc:request(put, {"http://localhost:8001/bjnortier/iphonedock/", [], "application/json", "{}"}, [], []),
    {[{<<"refs">>, {[{<<"master">>, <<"7b226368696c6472656e223a5b5d7d">>}]}}]}
	= jiffy:decode(iolist_to_binary(PostResponse)),

    %% Get design
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse1}} = 
	httpc:request(get, {"http://localhost:8001/bjnortier/iphonedock/", []}, [], []),
    {[{<<"refs">>, {[{<<"master">>, <<"7b226368696c6472656e223a5b5d7d">>}]}}]}
	= jiffy:decode(iolist_to_binary(GetResponse1)),
    
    %% Get ref
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse2}} = 
	httpc:request(get, {"http://localhost:8001/bjnortier/iphonedock/refs/master", []}, [], []),
    <<"7b226368696c6472656e223a5b5d7d">> = jiffy:decode(iolist_to_binary(GetResponse2)).
    

already_exists(_Config) ->
    create_new(_Config),
    {ok,{{"HTTP/1.1",409,_}, _, PostResponse}} = 
	httpc:request(put, {"http://localhost:8001/bjnortier/iphonedock/", [], "application/json", "{}"}, [], []),
    <<"already exists">> = jiffy:decode(iolist_to_binary(PostResponse)).

save(_Config) ->
    create_new(_Config),

    %% Invalid master update
    {ok,{{"HTTP/1.1",400,_}, _, PostResponse1}} = 
	httpc:request(put, {"http://localhost:8001/bjnortier/iphonedock/refs/master", [],  "application/json", jiffy:encode({[]})}, [], []),
    <<"only strings accepted">> = jiffy:decode(iolist_to_binary(PostResponse1)),

    %% Update master
    {ok,{{"HTTP/1.1",200,_}, _, PostResponse2}} = 
	httpc:request(put, {"http://localhost:8001/bjnortier/iphonedock/refs/master", [],  "application/json", jiffy:encode(<<"876abf32">>)}, [], []),
    <<"updated">> = jiffy:decode(iolist_to_binary(PostResponse2)),

    %% Get design
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse1}} = 
	httpc:request(get, {"http://localhost:8001/bjnortier/iphonedock/", []}, [], []),
    {[{<<"refs">>, {[{<<"master">>, <<"876abf32">>}]} }]}
	= jiffy:decode(iolist_to_binary(GetResponse1)),
    
    %% Get ref
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse2}} = 
	httpc:request(get, {"http://localhost:8001/bjnortier/iphonedock/refs/master", []}, [], []),
    <<"876abf32">> = jiffy:decode(iolist_to_binary(GetResponse2)).
    
