-module(node_worker_access_logger).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, accessed/1, expired/1, removed/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).

accessed(Hash) ->
    gen_server:cast(?MODULE, {accessed, Hash}).

expired(Seconds) ->
    gen_server:call(?MODULE, {expired, Seconds}).

removed(Hash) ->
    gen_server:cast(?MODULE, {removed, Hash}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-record(state, {access}).

init([]) ->
    State = #state{access = dict:new() },
    {ok, State}.

handle_call({expired, Seconds}, _From, State) ->
    ExpiredKeys = expired_keys(State#state.access, Seconds, erlang:now()),
    {reply, ExpiredKeys, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({accessed, Hash}, State) ->
    node_log:info("worker accessed [~p]~n", [Hash]),
    Now = erlang:now(),
    NewAccess = dict:store(Hash, Now, State#state.access),
    {noreply, State#state{ access = NewAccess }};
handle_cast({removed, Hash}, State) ->
    NewAccess = dict:erase(Hash, State#state.access),
    {noreply, State#state{ access = NewAccess }};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                          Private Functions                               %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


expired_keys(Dict, ExpirySecs, {NowMSec, NowSec, NowMicroSec}) ->
    dict:fold(fun(Key, {TsMSec, TsSec, TsMicroSec}, Acc) ->
		      
		      Diff = (NowMSec - TsMSec) * 1000000 +
			  (NowSec - TsSec) +
			  (NowMicroSec - TsMicroSec) / 1000000,
		      case Diff > ExpirySecs of
			  true ->
			      [Key|Acc];
			  false ->
			      Acc
		      end
	      end,
	      [],
	      Dict).

-ifdef(TEST).
expired_keys_test_() ->
    Dict0 = dict:new(),
    Dict1 = dict:store(a, {0, 0, 0}, Dict0),
    Dict2 = dict:store(b, {0, 1, 5000}, Dict1),
    Dict3 = dict:store(c, {3, 7, 9000}, Dict2),
    [
     ?_assertEqual(
	[],
	expired_keys(Dict3, 0, {0, 0, 0})),
     ?_assertEqual(
	[],
	expired_keys(Dict3, 1, {0, 1, 0})),
     ?_assertEqual(
	[a],
	expired_keys(Dict3, 1, {0, 2, 5000})),
     ?_assertEqual(
	[a, b],
	expired_keys(Dict3, 1, {0, 2, 5001})),
     ?_assertEqual(
	[a, b],
	expired_keys(Dict3, 0.001, {3, 7, 10000})),
     ?_assertEqual(
	[a, b, c],
	expired_keys(Dict3, 0.001, {3, 7, 10001}))
    ].
-endif.
