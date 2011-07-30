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

-module(node_worker).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([start/0, stop/0, init/1]).
-export([send/1]).

start() ->
    spawn(?MODULE, init, ["../worker/build/worker"]).
stop() ->
    complex ! stop.

send(X) ->
    call_port(X).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
	{complex, Result} ->
	    Result
    end.
init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn_executable, ExtPrg}, [{packet, 4}]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
            io:format("SEND: ~p~n", [Msg]),
	    Port ! {self(), {command, Msg}},
	    receive
		{Port, {data, Data}} ->
                    %%io:format("RECV: ~p~n", [Data]),
		    Caller ! {complex, Data}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
            io:format("port terminated: ~p~n", [Reason]),
	    exit(port_terminated);
        X ->
            io:format("received: ~p", [X]),
            loop(Port)
                
    end.
