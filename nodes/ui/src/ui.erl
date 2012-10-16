%% -*- mode: erlang -*-
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% 
%% Copyright (c) 2010-2012 Benjamin Nortier. All rights reserved
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

-module(ui).
-author('Benjamin Nortier <bjnortier@shapesmith.net>').
-export([start/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the ui server.
start() ->
    ensure_started(lager),
    ensure_started(mochiweb),
    ensure_started(folsom),
    application:set_env(webmachine, webmachine_logger_module, webmachine_logger),
    ensure_started(webmachine),
    application:start(ui).  