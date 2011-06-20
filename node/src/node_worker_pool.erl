-module(node_worker_pool).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2, stop/0, get_worker/0, call/2]).

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
	    ?MODULE ! {finished, WorkerPid},
	    Result
    end.

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
	    node_log:info("Waiting for worker... ~n"),
	    receive
		{finished, FinishedPid} ->
		    %% The worker may be dead after finishing, only add 
		    %% it to the available set if it is still alive
		    {Pid, NewState} = get_next(State#state{ available = [FinishedPid] }),
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
    {noreply, State#state{ available = [Pid|State#state.available]}};
handle_info({'EXIT', Pid, Reason}, State) ->
    node_log:error("Worker ~p exited. Reason: ~p~n", [Pid, Reason]),
    NewWorkerPid = create_worker(),
    Available = State#state.available,
    node_log:info("Available: ~p~n", [Available]),
    NewState = State#state{available = [NewWorkerPid|Available]},
    {noreply, NewState};
handle_info(Info, State) ->
    node_log:info("~p:handle_info: ~p~n", [?MODULE, Info]),
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
    node_log:info("Next worker: ~p~n", [Pid]),
    {Pid, State#state{ available = Remaining }}.
