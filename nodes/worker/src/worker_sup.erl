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

-module(worker_sup).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

    {ok, WorkerPath} = application:get_env(worker_executable),
    {ok, WorkerMaxTime} = application:get_env(worker_max_time),

    {ok, {{simple_one_for_one, 5, 10},
	  [{worker_process,
	    {worker_process, start_link, [WorkerPath, WorkerMaxTime]},
	    permanent, 5000, worker, [worker_process]}]}}.

