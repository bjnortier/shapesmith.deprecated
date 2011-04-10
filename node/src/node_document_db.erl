-module(node_document_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).
-export([create/0, exists/1, load/1, update/2, ids/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

exists(Id) ->
    gen_server:call(?MODULE, {exists, Id}).

load(Id) ->
    gen_server:call(?MODULE, {load, Id}).

update(Id, GeomIds) ->
    gen_server:call(?MODULE, {update, Id, GeomIds}).

create() ->
    gen_server:call(?MODULE, create).

ids() ->
    gen_server:call(?MODULE, ids).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

init([]) ->
    {ok, []}.

handle_call(ids, _From, State) ->
    {reply, State, State};
handle_call(create, _From, State) ->
    DocId = node_uuid:uuid(),
    io:format("creating document ~p:~p~n", [DocId, []]),
    ok = save_to_file(DocId, []),
    {reply, DocId, State};
handle_call({exists, DocId}, _From, State) ->
    Reply = file_exists(DocId),
    {reply, Reply, State};
handle_call({load, DocId}, _From, State) ->
    io:format("loading document ~p~n", [DocId]),
    GeomIds = load_from_file(DocId),
    {reply, GeomIds, State};
handle_call({update, DocId, GeomIds}, _From, State) ->
    io:format("updating document ~p:~p~n", [DocId, GeomIds]),
    ok = save_to_file(DocId, GeomIds),
    {reply, ok, State};
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
%%%                          Private Functions                               %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filename(DocId) ->
    {ok, DbDir} = application:get_env(node, db_dir),
    filename:join(
      [filename:dirname(code:which(?MODULE)),
       DbDir, DocId ++ ".doc"]).

file_exists(DocId) ->
    filelib:is_regular(filename(DocId)).

load_from_file(DocId) ->
    {ok, Contents} = file:read_file(filename(DocId)),
    GeomIds = binary_to_term(Contents),
    lists:map(fun(GeomId) ->
                      node_master:deserialize_geom(GeomId),
                      load_with_children(node_geom_db:geometry(GeomId))
              end,
              GeomIds),
    node_log:info("loaded geomIds: ~p~n", [GeomIds]),
    GeomIds.

save_to_file(DocId, GeomIds) ->
    ok = file:write_file(filename(DocId), term_to_binary(GeomIds)),
    node_log:info("serializing geom ids: ~p~n", [GeomIds]),
    lists:map(fun(GeomId) ->
                      save_with_children(GeomId, node_geom_db:geometry(GeomId))
              end,
              GeomIds),
    ok.

load_with_children(Geometry = {struct, Props}) ->
    node_log:info("Loading geometry: ~n~p~n", [Geometry]),
    case lists:keyfind(<<"children">>, 1, Props) of
        {<<"children">>, ChildIds} ->
            lists:map(fun(ChildIdBin) ->
                              ChildId = binary_to_list(ChildIdBin),
                              node_master:deserialize_geom(ChildId),
                              ChildGeometry = node_geom_db:geometry(ChildId),

                              load_with_children(ChildGeometry)
                      end,
                      ChildIds);
        false ->
            ok
    end.
    

save_with_children(GeomId, {struct, Props}) ->
    case lists:keyfind(<<"children">>, 1, Props) of
        {<<"children">>, ChildIds} ->
            lists:map(fun(ChildIdBin) ->
                              ChildId = binary_to_list(ChildIdBin),
                              ChildGeometry = node_geom_db:geometry(ChildId),
                              node_log:info("Saving child geometry: ~n~p~n", [ChildGeometry]),
                              save_with_children(ChildId, ChildGeometry)
                      end,
                      ChildIds);
        false ->
            ok
    end,
    ok = node_master:serialize_geom(GeomId),
    ok = node_master:serialize_brep(GeomId),
    ok.
    

            
