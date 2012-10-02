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

-module(api_deps).
-export([start_with_api/0, start_with_api/1, start_without_api/0]).
-export([stop_with_api/0, stop_without_api/0]).

-define(APPS, [inets, crypto, bcrypt, lager, mochiweb, webmachine, folsom]).

start_with_api() ->
    start_with_api(fun() -> ok end).                   

start_with_api(APIInitFun) ->
    start_without_api(),

    ok = application:load(api),
    ok = application:set_env(api, port, 8001),
    ok = application:set_env(api, db_module, api_mem_db),
    ok = APIInitFun(),
    ok = application:start(api),
    ok.

start_without_api() ->
    AllOk = lists:duplicate(length(?APPS), ok),
    AllOk = lists:map(fun application:start/1, ?APPS),

    ok = application:start(worker_master),
    ok = application:load(worker),
    ok = application:set_env(worker, worker_master_node, node()),
    ok = application:start(worker).


stop_with_api() ->
    ok = application:stop(api),
    ok = application:unload(api),

    stop_without_api().

stop_without_api() ->
    ok = application:stop(worker_master),
    ok = application:stop(worker),
    ok = application:unload(worker),

    AllOk = lists:duplicate(length(?APPS), ok),
    AllOk = lists:map(fun application:stop/1, lists:reverse(?APPS)),
    ok.
