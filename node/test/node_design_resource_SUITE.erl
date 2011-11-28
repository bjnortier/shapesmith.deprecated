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
	 create_new
	 %already_exists,
	 %save
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

malformed_create(_Config) ->
    {ok,{{"HTTP/1.1",400,_}, ResponseHeaders1, PostResponse1}} = 
	httpc:request(post, {"http://localhost:8001/bjnortier/iphonedock/", [], "application/json", "&@^%£$"}, [], []),
    check_json_content_type(ResponseHeaders1),
    <<"invalid JSON">> = jiffy:decode(iolist_to_binary(PostResponse1)),

    {ok,{{"HTTP/1.1",400,_}, ResponseHeaders2, PostResponse2}} = 
	httpc:request(post, {"http://localhost:8001/bjnortier/iphonedock/", [], "application/json", "[]"}, [], []),
    <<"only {} accepted">> = jiffy:decode(iolist_to_binary(PostResponse2)),
    check_json_content_type(ResponseHeaders2),
    
    ok.


not_found(_Config) ->
    {ok,{{"HTTP/1.1",404,_}, GetResponseHeaders, GetResponse}} = 
	httpc:request(get, {"http://localhost:8001/bjnortier/undefined/", []}, [], []),
    check_json_content_type(GetResponseHeaders),
    <<"not found">> = jiffy:decode(iolist_to_binary(GetResponse)).

create_new(_Config) ->
    %% Create
    {ok,{{"HTTP/1.1",200,_}, Headers1, PostResponse1}} = 
	httpc:request(post, {"http://localhost:8001/bjnortier/iphonedock/", [], "application/json", "{}"}, [], []),
    check_json_content_type(Headers1),
    {[{<<"refs">>,
       {[{<<"heads">>,
	  {[{<<"master">>,
	     <<"009c525d16ccfc06e955ad89766707da9a68504a">>}]}}]}}]}
	= jiffy:decode(iolist_to_binary(PostResponse1)),

    %% Cannot create more than once
    {ok,{{"HTTP/1.1",400,_}, Headers2, PostResponse2}} = 
	httpc:request(post, {"http://localhost:8001/bjnortier/iphonedock/", [], "application/json", "{}"}, [], []),
    check_json_content_type(Headers2),
    <<"already exists">> = jiffy:decode(iolist_to_binary(PostResponse2)),

    %% Get design
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse1}} = 
	httpc:request(get, {"http://localhost:8001/bjnortier/iphonedock/", []}, [], []),
    {[{<<"refs">>,
       {[{<<"heads">>,
	  {[{<<"master">>,
	     <<"009c525d16ccfc06e955ad89766707da9a68504a">>}]}}]}}]}
	= jiffy:decode(iolist_to_binary(GetResponse1)),
    
    %% Get ref
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse2}} = 
	httpc:request(get, {"http://localhost:8001/bjnortier/iphonedock/refs/master", []}, [], []),
    <<"7b226368696c6472656e223a5b5d7d">> = jiffy:decode(iolist_to_binary(GetResponse2)).

    
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


check_json_content_type(Headers) ->
    {Headers, {_, "application/json"}} = {Headers, lists:keyfind("content-type", 1, Headers)}.

    
