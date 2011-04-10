-module(node_worker).
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
