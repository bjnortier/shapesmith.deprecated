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

-module(api_stl_resource_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
	 export,
	 forbidden
	].

init_per_suite(Config) ->
    api_deps:start_with_api(fun() ->
                                    ok = application:set_env(api, auth_module, api_session_auth),
                                    ok = application:set_env(api, host, "http://localhost.shapesmith.net:8001")
                            end),
    Config.

end_per_suite(_Config) ->
    api_deps:stop_with_api(),
    ok.

init_per_testcase(_, Config) ->
    {ok, _} = api_mem_db:start_link(),
    Config.

end_per_testcase(_Testcase, _Config) ->
    api_mem_db:stop(),
    ok.


export(_Config) ->

    %% Signup
    httpc:set_options([{cookies, enabled}]),
    PostBody = jiffy:encode({[{<<"username">>, <<"bjnortier">>},
			      {<<"password1">>, <<"abc">>},
			      {<<"password2">>, <<"abc">>}
			     ]}),
    
    {ok,{{"HTTP/1.1",201,_}, _, _}} = 
	httpc:request(post, {"http://localhost.shapesmith.net:8001/signup", [], "application/json", PostBody}, [], []),
    
    %% Not found
    {ok,{{"HTTP/1.1",404,_}, _, _NotFoundResponse}} = 
      	httpc:request(get, {"http://localhost.shapesmith.net:8001/bjnortier/iphonedock/stl/abc", []}, [], []),

    
    %% Create Geom
    GeomCreateURL = "http://localhost.shapesmith.net:8001/bjnortier/iphonedock/geom/", 
    GeomA = {[{<<"type">>, <<"cuboid">>},
              {<<"origin">>, {[{<<"x">>, 0}, {<<"y">>, 0}, {<<"z">>, 0}]}},
              {<<"parameters">>, {[{<<"u">>, 1}, {<<"v">>, 1}, {<<"w">>, 1}]}}]},
    
    {ok,{{"HTTP/1.1",200,_}, _, ResponseA}} = 
	httpc:request(post, {GeomCreateURL, [], "application/json", jiffy:encode(GeomA)}, [], []),
    {[{<<"path">>, _},
      {<<"SHA">>, GeomSHABin}]} = jiffy:decode(iolist_to_binary(ResponseA)),

    %% Create commit
    Commit = {[{<<"parent">>, <<"not_used">>},
               {<<"geoms">>, [GeomSHABin]}]},
    CommitCreateURL = "http://localhost.shapesmith.net:8001/bjnortier/iphonedock/commit/", 
    {ok,{{"HTTP/1.1",200,_}, _, PostResponse}} = 
	httpc:request(post, {CommitCreateURL, [], "application/json", jiffy:encode(Commit)}, [], []),
    {[{<<"path">>, _},
      {<<"SHA">>, CommitSHABin}]} = jiffy:decode(iolist_to_binary(PostResponse)),
    CommitSHA = binary_to_list(CommitSHABin),
    
    %% Get
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse}} = 
     	httpc:request(get, {"http://localhost.shapesmith.net:8001/bjnortier/iphonedock/stl/" ++ CommitSHA, []}, [], []),
    "solid " ++ _ = GetResponse,

    %% Cannot get before publishing
    httpc:reset_cookies(),
    {ok,{{"HTTP/1.1",403,_}, _, _}} = 
     	httpc:request(get, {"http://localhost.shapesmith.net:8001/bjnortier/iphonedock/stl/" ++ CommitSHA, []}, [], []),

    %% Sign in
    SigninPostBody = jiffy:encode({[{<<"username">>, <<"bjnortier">>},
				     {<<"password">>, <<"abc">>}
				    ]}),
    {ok,{{"HTTP/1.1",201,_}, _, _}} = 
	httpc:request(post, {"http://localhost.shapesmith.net:8001/signin", [], "application/json", SigninPostBody}, [], []),

    %% Publish
    {ok,{{"HTTP/1.1",200,_}, _, _}} = 
	httpc:request(post, {"http://localhost.shapesmith.net:8001/bjnortier/iphonedock/stl/publish/" ++ CommitSHA, [], "application/json", "{}"}, [], []),
    
    %% Can access without auth
    httpc:reset_cookies(),
    {ok,{{"HTTP/1.1",200,_}, _, _}} = 
     	httpc:request(get, {"http://localhost.shapesmith.net:8001/bjnortier/iphonedock/stl/" ++ CommitSHA, []}, [], []),

    ok.
    
forbidden(_Config) ->
    {ok,{{"HTTP/1.1",403,_}, _, _NotFoundResponse}} = 
      	httpc:request(get, {"http://localhost:8001/bjnortier/iphonedock/stl/abc", []}, [], []).

    
    
