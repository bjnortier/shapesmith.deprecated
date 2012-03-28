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

-module(node_commit_resource_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
	 validation,
	 creation
	].

init_per_suite(Config) ->
    ok = application:load(node),
    application:set_env(node, port, 8001),
    application:set_env(node, db_module, node_mem_db),
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

validation(_Config) ->
    Pairs = [{ <<"abd45">>, <<"commit must be an object">>},
             { {[{<<"geoms">>, [<<"ab13">>]}]}, <<"no parent commit specified">>},
             { {[{<<"geoms">>, []}, {<<"parent">>, []}]}, <<"parent commit must be a string">>},
             { {[{<<"parent">>, <<"ab13">>}]}, <<"no geoms specified">>},
             { {[{<<"geoms">>, <<"ab18276c">>}]}, <<"geoms must be an array">>},
             { {[{<<"geoms">>, []}, {<<"parent">>, <<"a12">>}, {<<"hidden">>, <<"f12">>}]}, <<"hidden must be an array">>}
            ],
    
    lists:map(fun({Commit, ValidationError}) ->
                      URL = "http://localhost:8001/bjnortier/iphonedock/commit/",
                      
                      {ok,{{"HTTP/1.1",400,_}, Headers, Response}} = 
                      httpc:request(post, {URL, [], "application/json", jiffy:encode(Commit)}, [], []),
                      check_json_content_type(Headers),
                      {[{<<"validation">>, ValidationError}]} 
                          = jiffy:decode(iolist_to_binary(Response))
              end,
              Pairs).

creation(_Config) ->

    %% Create geometry
    Commit = {[{<<"parent">>, <<"abc56">>},
               {<<"geoms">>, [<<"ab3f1">>, <<"9a0eb">>]}]},
    CreateURL = "http://localhost:8001/local/iphonedock/commit/", 
    {ok,{{"HTTP/1.1",200,_}, CreateHeaders, PostResponse}} = 
	httpc:request(post, {CreateURL, [], "application/json", jiffy:encode(Commit)}, [], []),

    check_json_content_type(CreateHeaders),
    {[{<<"path">>, PathBin},
      {<<"SHA">>, SHABin}]} = jiffy:decode(iolist_to_binary(PostResponse)),
    Path = binary_to_list(PathBin),
    SHA = binary_to_list(SHABin),
    "/local/iphonedock/commit/" ++ SHA = Path,

    %% Get the created commit
    {ok,{{"HTTP/1.1",200,_}, GetHeaders, GetResponse}} = 
	httpc:request(get, {"http://localhost:8001" ++ Path, []}, [], []),
    check_json_content_type(GetHeaders),

    %% It's the same as the original
    Commit = jiffy:decode(iolist_to_binary(GetResponse)),

    ok.

check_json_content_type(Headers) ->
    {Headers, {_, "application/json"}} = {Headers, lists:keyfind("content-type", 1, Headers)}.

