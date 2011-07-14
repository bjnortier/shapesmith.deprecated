-module(node_mem_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).
-export([exists/2, get/2, create/2, put/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).

exists(Bucket, Id) ->
    gen_server:call(?MODULE, {exists, Bucket, Id}).

get(Bucket, Id) ->
    gen_server:call(?MODULE, {get, Bucket, Id}).

create(Bucket, Value) ->
    gen_server:call(?MODULE, {create, Bucket, Value}).

put(Bucket, Id, Value) ->
    gen_server:call(?MODULE, {put, Bucket, Id, Value}).
    

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
handle_call({create, Bucket, Value}, _From, State) ->
    Id = node_uuid:uuid(),
    NewState = [{{Bucket, Id}, Value}|State],
    {reply, Id, NewState};
handle_call({put, Bucket, Id, Value}, _From, State) ->
    NewState = case lists:keyfind({Bucket, Id}, 1, State) of
		   {{Bucket, Id}, _} -> 
		       lists:keyreplace({Bucket, Id}, 1, State, {{Bucket, Id}, Value});
		   false -> 
		       [{{Bucket, Id}, Value}|State]
	       end,
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
