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

-module(worker_master_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{seconds,5}}].

all() ->
	[
	 queue_dequeue,
	 waiting,
	 dead_workers,
	 monitor
	].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    ok = application:start(folsom),
    ok = application:start(worker_master),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(folsom),
    ok = application:stop(worker_master).

%% ===================================================================
%% Tests
%% ===================================================================


queue_dequeue(_) ->
    Pid1 = spawn_dummy_worker(),
    Pid2 = spawn_dummy_worker(),
    
    ok = worker_master_pool:put_worker(Pid1),
    ok = worker_master_pool:put_worker(Pid2),

    Pid1 = worker_master_pool:get_worker(0),
    Pid2 = worker_master_pool:get_worker(0),
    
    {error, no_worker_available} = worker_master_pool:get_worker(0).

waiting(_) ->
    {Time, _} = timer:tc(
		  fun() ->
			  {error, no_worker_available} = worker_master_pool:get_worker(1)
		  end),
    true = Time > 1000000,

    Pid = spawn_dummy_worker(),

    %% A waiting process will get an inserted process
    TestPid = self(),
    spawn(fun() ->
		  TestPid ! worker_master_pool:get_worker(5)
	  end),

    ok = worker_master_pool:put_worker(Pid),
    Pid = receive X -> X end.

dead_workers(_) ->
    Pid1 = spawn_dead_worker(),
    Pid2 = spawn_dummy_worker(),
    
    ok = worker_master_pool:put_worker(Pid1),
    ok = worker_master_pool:put_worker(Pid2),

    Pid2 = worker_master_pool:get_worker(0),
    {error, no_worker_available} = worker_master_pool:get_worker(0).

monitor(_) ->
    Pid1 = spawn_dummy_worker(),
    ok = worker_master_pool:put_worker(Pid1),
    exit(Pid1, kill),
    timer:sleep(100),
    {error, no_worker_available} = worker_master_pool:get_worker(0).   

%% ===================================================================
%% Utils
%% ===================================================================

spawn_dummy_worker() ->
    spawn(fun() -> receive X -> X end end).

spawn_dead_worker() ->
    spawn(fun() -> ok end).

		  
