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

-module(worker_app).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = worker_sup:start_link(),

    %% Join the master node if this is a distributed worker application
    {ok, Node} = application:get_env(worker_master_node),
    case net_adm:ping(Node) of 
        pong ->
            lager:info("worker node ~p joined master node ~p", [node(), Node]);
        pang ->
            lager:error("could not join master worker node ~p", [Node]),
            throw(could_not_join_worker_master)
    end,
    ok = global:sync(),

    %% Spawn the workers
    {ok, NumberOfWorkers} = application:get_env(number_of_workers),
    lists:map(fun(_) ->
                      {ok, _} = supervisor:start_child(worker_sup, [])
              end,
              lists:seq(1, NumberOfWorkers)),
    {ok, Pid}.

stop(_State) ->
    ok.
