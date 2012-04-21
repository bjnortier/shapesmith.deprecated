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

-module(worker_process).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2, stop/0, safe_call/2, get_stl_and_delete/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(WorkerPath, WorkerMaxTime) ->
    gen_server:start_link(?MODULE, [WorkerPath, WorkerMaxTime], []).
stop() ->
    gen_server:call(?MODULE, stop).


safe_call(WorkerPid, Msg) ->
    case catch(call(WorkerPid, Msg)) of
        {'EXIT', {{error, Reason}, _}} ->
            {error, Reason};
        Result ->
	    Result
    end.

call(Pid, Msg) when is_list(Msg) andalso length(Msg) > 0 ->
    call(Pid, list_to_binary(Msg));
call(_Pid, Msg) when is_list(Msg)  ->
    {error, empty_msg};
call(Pid, Msg) when is_binary(Msg) andalso size(Msg) > 0 ->
    gen_server:call(Pid, {call, Msg}, infinity);
call(_Pid, Msg) when is_binary(Msg) ->
    {error, empty_msg};
call(_, _Msg) ->
    {error, msg_must_be_a_list_or_binary}.


get_stl_and_delete(Pid, Filename) ->
    try 
        gen_server:call(Pid, {get_stl_and_delete, Filename})
    catch 
        Err:Reason ->
            {error, {Err, Reason}}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-record(state, {port, worker_max_time}).

init([WorkerPath, WorkerMaxTime]) ->
    WorkerBin = filename:absname(filename:join([code:priv_dir(worker)] ++ WorkerPath)),
    lager:info("starting worker process for: ~p", [WorkerBin]),
    process_flag(trap_exit, true),
    Port = open_port({spawn_executable, WorkerBin}, [{packet, 4}, 
                                                     {cd, filename:dirname(WorkerBin)}]),
    gen_server:call(global:whereis_name(worker_master_pool), {put_worker, self()}),
    {ok, #state{port = Port, worker_max_time = WorkerMaxTime}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({call, Msg}, _From, State) ->
    Port = State#state.port,
    WorkerMaxTime = State#state.worker_max_time,
    Port ! {self(), {command, Msg}},
    receive
        {Port, {data, Data}} ->
	    {reply, list_to_binary(Data), State};
        {'EXIT', Port, Reason} ->
            {stop, {error, Reason}, State}
    after WorkerMaxTime ->
	    {stop, {error, worker_timeout}, State}
    end;
handle_call({get_stl_and_delete, Filename}, _From, State) ->
    Result = file:read_file(Filename),
    file:delete(Filename),
    {reply, Result, State};
handle_call(Request, _From, State) ->
    lager:warning("~p unknown call: ~p", [?MODULE, Request]),
    {reply, unknown_call, State}.

handle_cast(Msg, State) ->
    lager:warning("~p unknown cast: ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, State) when Port =:= State#state.port->
    lager:warning("~p port has exited: ~p", [?MODULE, Reason]),
    {stop, port_process_terminated, State};
handle_info(Info, State) ->
    lager:warning("~p unknown info: ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:info("terminating ~p: ~p", [?MODULE, Reason]),
    Port = State#state.port,
    Port ! {self(), close},
    receive 
        {Port, closed} ->
            ok;
        Err ->
            throw(Err)
    after 5000 ->
            throw(no_close_received)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



