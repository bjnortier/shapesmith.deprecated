%% TODO: Need error return design for the worker, e.g. when there is a problem
%% with the request. E.g. {"error" : <reason>}


-module(node_worker_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, call/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).

call(Msg) when is_list(Msg) andalso length(Msg) > 0 ->
    gen_server:call(?MODULE, {call, Msg}, 30000);
call(Msg) when is_list(Msg)  ->
    empty_msg;
call(Msg) when is_binary(Msg) andalso size(Msg) > 0 ->
    gen_server:call(?MODULE, {call, Msg}, 30000);
call(Msg) when is_binary(Msg) ->
    empty_msg;
call(_Msg) ->
    msg_must_be_a_list_or_binary.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-record(state, {port}).

init([]) ->
    {ok, WorkerPath} = application:get_env(node, worker_executable),
    WorkerBin = filename:join(
                  [filename:dirname(code:which(?MODULE))|WorkerPath]),
    process_flag(trap_exit, true),
    Port = open_port({spawn_executable, WorkerBin}, [{packet, 4}]),
    {ok, #state{port = Port}}.

handle_call(stop, _From, State) ->
    Reason = normal,
    Reply = stopped,
    {stop, Reason, Reply, State};
handle_call({call, Msg}, _From, State) ->
    %%io:format("MSG: ~p~n", [Msg]),
    Port = State#state.port,
    Port ! {self(), {command, Msg}},
    receive
        {Port, {data, Data}} ->
            {reply, Data, State};
        {'EXIT', Port, Reason} ->
            {stop, {error, Reason}, State}
    after 30000 ->
            {reply, {error, timeout}, State}
    end;
handle_call(Request, _From, State) ->
    io:format("UNKNOWN node_worker_server:handle_call(~p)~n", [Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, State) when Port =:= State#state.port->
    io:format("node_worker_server port has exited: ~p~n", [Reason]),
    {stop, port_process_terminated, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    io:format("node_worker_server:terminate(~p)~n", [Reason]),
    Port = State#state.port,
    Port ! {self(), close},
    receive 
        {Port, closed} ->
            ok;
        X ->
            throw(X)
    after 10000 ->
            throw(no_close_received)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



