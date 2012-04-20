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

-module(node_mem_db).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, start_link/0, stop/0]).
-export([exists/2, get/2, put/3, delete/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).

exists(Bucket, Id) ->
    gen_server:call(?MODULE, {exists, Bucket, Id}).

get(Bucket, Id) ->
    gen_server:call(?MODULE, {get, Bucket, Id}).

put(Bucket, Id, Value) ->
    gen_server:call(?MODULE, {put, Bucket, Id, Value}).

delete(Bucket, Id) ->
    gen_server:call(?MODULE, {delete, Bucket, Id}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

init([]) ->
    {ok, []}.

handle_call({exists, Bucket, Id}, _From, State) ->
    Reply = case lists:keyfind({Bucket, Id}, 1, State) of
                {{Bucket, Id}, _Value} -> true;
                false -> false
            end,
    {reply, Reply, State};
handle_call({get, Bucket, Id}, _From, State) ->
    Reply = case lists:keyfind({Bucket, Id}, 1, State) of
                {{Bucket, Id}, Value} -> Value;
                false -> undefined
            end,
    {reply, Reply, State};
handle_call({put, Bucket, Id, Value}, _From, State) ->
    NewState = case lists:keyfind({Bucket, Id}, 1, State) of
		   {{Bucket, Id}, _} -> 
		       lists:keyreplace({Bucket, Id}, 1, State, {{Bucket, Id}, Value});
		   false -> 
		       [{{Bucket, Id}, Value}|State]
	       end,
    {reply, ok, NewState};
handle_call({delete, Bucket, Id}, _From, State) ->
    NewState = lists:keydelete({Bucket, Id}, 1, State),
    {reply, ok, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
