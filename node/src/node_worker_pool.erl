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

-module(node_worker_pool).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2, stop/0, get_worker/0, call/2, return_worker/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(PoolSize, WorkerMaxTime) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PoolSize, WorkerMaxTime], []).
stop() ->
    gen_server:call(?MODULE, stop).

get_worker() ->
    gen_server:call(?MODULE, get_worker).

call(WorkerPid, Msg) ->
    case catch(node_worker_server:call(WorkerPid, Msg)) of
	{'EXIT', {{error, Reason}, _}} ->
	    {error, Reason};
	Result ->
	    Result
    end.

return_worker(WorkerPid) ->
    ?MODULE ! {finished, WorkerPid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-record(state, {available, worker_max_time}).

init([PoolSize, WorkerMaxTime]) ->
    process_flag(trap_exit, true),
    Available = [create_worker() || _ <- lists:seq(1,PoolSize)],
    {ok, #state{available = Available, worker_max_time = WorkerMaxTime}}.

handle_call(get_worker, _From, State) ->
    Available = State#state.available,
    WorkerMaxTime = State#state.worker_max_time,
    case Available of
	[] ->
	    lager:info("Waiting for worker... ~n"),
	    receive
		{finished, FinishedPid} ->
		    {Pid, NewState} = get_next(return_worker_if_alive(State, FinishedPid)),
		    {reply, Pid, NewState}
	    after WorkerMaxTime ->
		    {reply, {error, worker_timeout}, State}
	    end;
	_ ->
	    {Pid, NewState} = get_next(State),
	    {reply, Pid, NewState}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({finished, Pid}, State) ->
    {noreply, return_worker_if_alive(State, Pid)};
handle_info({'EXIT', Pid, Reason}, State) ->
    lager:error("Worker ~p exited. Reason: ~p~n", [Pid, Reason]),
    NewWorkerPid = create_worker(),
    Available = State#state.available,
    lager:info("Available: ~p~n", [Available]),
    NewState = State#state{available = [NewWorkerPid|Available]},
    {noreply, NewState};
handle_info(Info, State) ->
    lager:info("~p:handle_info: ~p~n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Private                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


create_worker() ->
    {ok, Pid} = supervisor:start_child(node_worker_sup, []),
    link(Pid),
    Pid.

get_next(State) ->
    [Pid|Remaining] = State#state.available,
    lager:info("Next worker: ~p~n", [Pid]),
    {Pid, State#state{ available = Remaining }}.

return_worker_if_alive(State, Pid) ->
    case is_process_alive(Pid) of
	true ->
	    lager:info("Returning worker ~p~n", [Pid]),
	    State#state{ available = [Pid|State#state.available]};
	false ->
	    lager:info("Ignoring dead worker ~p~n", [Pid]),
	    State
    end.

