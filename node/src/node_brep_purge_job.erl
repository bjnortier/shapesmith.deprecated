-module(node_brep_purge_job).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link(JobPeriod, BRepExpirySecs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [JobPeriod, BRepExpirySecs], []).
stop() ->
    gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

init([JobPeriod, BRepExpirySecs]) ->
    process_flag(trap_exit, true),
    Pid = erlang:spawn_link(fun() -> loop(JobPeriod, BRepExpirySecs) end),
    {ok, {Pid}}.

loop(JobPeriod, BRepExpirySecs) ->
    receive
	stop ->
	    stopped
    after JobPeriod ->
	    lists:map(fun(Hash) ->
			      Msg = {struct, [{<<"purge">>, list_to_binary(Hash)}]},
			      case node_worker_server:call(mochijson2:encode(Msg)) of
				  "true" -> 
				      node_worker_access_logger:removed(Hash),
				      node_log:info("Purged expired BREP: ~p~n", [Hash]);
				  X ->
				      node_log:info("Error purging expired BREP: ~p Reason: ~p~n", [Hash, X])
			      end
		      end,
		      node_worker_access_logger:expired(BRepExpirySecs)),
	    loop(JobPeriod, BRepExpirySecs)
    end.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT',Pid,Reason}, State = {Pid}) ->
    node_log:info("Exit received from timer loop. Reason: ~p~n", [Reason]),
    {stop, timer_exit, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, {LoopPid}) ->
    LoopPid ! stop,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



