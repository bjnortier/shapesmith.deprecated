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

-module(node_geom_resource_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
	 creation,
	 validation
	].

init_per_suite(Config) ->
    ok = application:load(node),
    application:set_env(node, port, 8001),
    application:set_env(node, db_module, node_mem_db),
    application:start(inets),
    application:start(lager),
    ok = node:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(lager),
    application:stop(inets),
    application:stop(node),
    application:unload(node),
    ok.

init_per_testcase(_Testcase, Config) ->
    {ok, _} = node_mem_db:start_link(),
    Config.

end_per_testcase(_Testcase, _Config) ->
    node_mem_db:stop(),
    ok.

creation(_Config) ->

    %% Create geometry
    GeomJSON = {[{<<"type">>, <<"sphere">>},
		 {<<"origin">>, {[{<<"x">>, 0},
				  {<<"y">>, 0},
				  {<<"z">>, 0}]}},
		 {<<"parameters">>, {[{<<"r">>, 1.1}]}}]},
    CreateURL = "http://localhost:8001/bjnortier/iphonedock/geom/", 
    {ok,{{"HTTP/1.1",200,_}, CreateHeaders, PutResponse}} = 
	httpc:request(post, {CreateURL, [], "application/json", jiffy:encode(GeomJSON)}, [], []),

    check_json_content_type(CreateHeaders),
    {[{<<"path">>, PathBin}]} = jiffy:decode(iolist_to_binary(PutResponse)),
    Path = binary_to_list(PathBin),
    "/bjnortier/iphonedock/geom/" ++ _SHA = Path,

    %% Get the created geometry
    {ok,{{"HTTP/1.1",200,_}, GetHeaders, GetResponse}} = 
	httpc:request(get, {"http://localhost:8001" ++ Path, []}, [], []),
    check_json_content_type(GetHeaders),

    %% It's the same as the original
    GeomJSON = jiffy:decode(iolist_to_binary(GetResponse)),

    ok.

validation(_Config) ->

    GeomJSON = {[{<<"type">>, <<"sphere">>},
		 {<<"origin">>, {[{<<"x">>, 0},
				  {<<"y">>, 0},
				  {<<"z">>, 0}]}},
		 {<<"parameters">>, {[{<<"r">>, -1.0}]}}]},
    EncodedJSON = jiffy:encode(GeomJSON),
    URL = "http://localhost:8001/bjnortier/iphonedock/geom/",
    
    {ok,{{"HTTP/1.1",400,_}, Headers, PutResponse}} = 
     	httpc:request(post, {URL, [], "application/json", EncodedJSON}, [], []),
    check_json_content_type(Headers),
    
    {[{<<"validation">>, {[{<<"r">>, <<"must be positive">>}]}}]} 
     	= jiffy:decode(iolist_to_binary(PutResponse)),
    ok.

check_json_content_type(Headers) ->
    {Headers, {_, "application/json"}} = {Headers, lists:keyfind("content-type", 1, Headers)}.

