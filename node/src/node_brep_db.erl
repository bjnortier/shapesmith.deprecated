-module(node_brep_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).
-export([exists/1, create/2]).
-export([log/0]).
-export([serialize/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).

exists(Hash) ->
    gen_server:call(?MODULE, {exists, Hash}, 30000).

create(Hash, Geometry) ->
    gen_server:call(?MODULE, {create, Hash, Geometry}, 30000).

log() ->
    gen_server:call(?MODULE, log, 30000).

serialize(Hash) ->
    gen_server:call(?MODULE, {serialize, Hash}, 30000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-record(state, {log}).

init([]) ->
    State = #state{log = dict:new() },
    {ok, State}.

handle_call({exists, Hash}, _From, State) ->
    node_worker_access_logger:accessed(Hash),

    Msg = {struct, [{<<"exists">>, list_to_binary(Hash)}]},
    Reply = case node_worker_server:call(mochijson2:encode(Msg)) of
                "true" -> true;
                "false" -> false;
                X -> throw({unknown_exists_reponse, X})
            end,
    {reply, Reply, State};

handle_call({create, Hash, Geometry}, _From, State) ->
    node_worker_access_logger:accessed(Hash),

    %% If the serialized BREP exists, use that. Otherwise
    %% create using the worker
    BREPFilename = brep_filename(Hash),
    case filelib:is_regular(BREPFilename) of
        true ->

            node_log:info("Creating BREP for ~p from file~n", [Hash]),

            {ok, S11N} = file:read_file(BREPFilename),
            %% Insert into worker
            Msg = {struct, [{<<"type">>, <<"deserialize">>},
                            {<<"id">>, list_to_binary(Hash)},
                            {<<"s11n">>, S11N}]},
            case node_worker_server:call(mochijson2:encode(Msg)) of
		"\"ok\"" ->
		    NewLog = dict:store(Hash, from_serialized, State#state.log),
		    NewState = State#state{ log = NewLog },
		    {reply, ok, NewState};
		ErrorMsg ->
		    {reply, {error, ErrorMsg}, State}
	    end;

        false ->
            
            node_log:info("Creating BREP for ~p using worker~n", [Hash]),

            {struct, GeomProps} = Geometry,
            {<<"type">>, GeomType} = lists:keyfind(<<"type">>, 1, GeomProps),
	    case create_type(Hash, GeomType, Geometry) of
		"\"ok\"" ->
		    NewLog = dict:store(Hash, from_worker, State#state.log),
		    NewState = State#state{ log = NewLog },
		    {reply, ok, NewState};
		ErrorMsg ->
		    {reply, {error, ErrorMsg}, State}
	    end
    end;

handle_call({serialize, Hash}, _From, State) ->
    Msg = {struct, [{<<"type">>, <<"serialize">>},
                    {<<"id">>, list_to_binary(Hash)}]},
    {struct, [{<<"s11n">>, S11N}]} = mochijson2:decode(node_worker_server:call(mochijson2:encode(Msg))),
    
    BREPFilename = brep_filename(Hash),
    node_log:info("writing brep for ~p to ~s~n", [Hash, BREPFilename]),
    ok = file:write_file(BREPFilename, S11N),
    {reply, ok, State};

handle_call(log, _From, State) ->
    Log = dict:to_list(State#state.log),
    {reply, Log, State};

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 private                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


brep_filename(Hash) ->
    {ok, DbDir} = application:get_env(node, db_dir),
    filename:join(
      [filename:dirname(code:which(?MODULE)),
       DbDir, Hash ++ ".brep"]).

create_type(Hash, <<"union">>, Geometry) ->
    create_boolean(Hash, <<"union">>, Geometry);
create_type(Hash, <<"subtract">>, Geometry) ->
    create_boolean(Hash, <<"subtract">>, Geometry);
create_type(Hash, <<"intersect">>, Geometry) ->
    create_boolean(Hash, <<"intersect">>, Geometry);
%% Non-bool pass through
create_type(Hash, _, Geometry) ->
    worker_create(Hash, Geometry).

create_boolean(Hash, Type, Geometry) ->
    {struct, GeomProps} = Geometry,
    {<<"children">>, ChildIds} = lists:keyfind(<<"children">>, 1, GeomProps),
    ChildHashes = lists:map(fun(ChildId) ->
                                    ChildHash = node_geom_db:hash(binary_to_list(ChildId)),
                                    list_to_binary(ChildHash)
                            end,
                            ChildIds),

    Transforms = case lists:keyfind(<<"transforms">>, 1, GeomProps) of
                     false -> [];
                     {<<"transforms">>, T} -> T
                 end,
    worker_create(Hash, {struct, [{<<"type">>, Type},
                                  {<<"children">>, ChildHashes},
                                  {<<"transforms">>, Transforms}
                                 ]}).
worker_create(Hash, Geometry) ->
    Msg = {struct, [{<<"type">>, <<"create">>},
                    {<<"id">>, list_to_binary(Hash)},
                    {<<"geometry">>, Geometry}
                   ]},
    node_worker_server:call(mochijson2:encode(Msg)).
