-module(node_master).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).
-export([create_geom/1, update_geom/2, mesh_geom/1, exists/1, geometry/1, recursive_geometry/1, stl/1]).
-export([serialize_geom/1, deserialize_geom/1]).
-export([serialize_brep/1, deserialize_brep/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

exists(Id) ->
    gen_server:call(?MODULE, {exists, Id}, 30000).

create_geom(Geometry) ->
    gen_server:call(?MODULE, {create_geom, Geometry}, 30000).
update_geom(Id, Geometry) ->
    gen_server:call(?MODULE, {update_geom, Id, Geometry}, 30000).


geometry(Id) ->
    gen_server:call(?MODULE, {geometry, Id}, 30000).

recursive_geometry(Id) ->
    gen_server:call(?MODULE, {recursive_geometry, Id}, 30000).

serialize_geom(Id) ->
    gen_server:call(?MODULE, {serialize_geom, Id}, 30000).

deserialize_geom(Id) ->
    gen_server:call(?MODULE, {deserialize_geom, Id}, 30000).

serialize_brep(Id) ->
    gen_server:call(?MODULE, {serialize_brep, Id}, 30000).

deserialize_brep(Id) ->
    gen_server:call(?MODULE, {deserialize_brep, Id}, 30000).


mesh_geom(Id) -> 
    gen_server:call(?MODULE, {mesh_geom, Id}, 30000).

stl(Id) ->
    gen_server:call(?MODULE, {stl, Id}, 30000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

init([]) ->
    {ok, []}.

handle_call({exists, Id}, _From, State) ->
    Reply = node_geom_db:exists(Id),
    {reply, Reply, State};
handle_call({geometry, Id}, _From, State) ->
    Reply = node_geom_db:geometry(Id),
    {reply, Reply, State};
handle_call({recursive_geometry, Id}, _From, State) ->
    Reply = node_geom_db:recursive_geometry(Id),
    {reply, Reply, State};
handle_call({create_geom, Geometry}, _From, State) ->
    case node_geom_db:create(Geometry) of
	{ok, Id} ->
	    Hash = node_geom_db:hash(Id),
	    case ensure_brep_exists(Id, Geometry, Hash, fun() -> ok end) of
		ok ->
		    {reply, {ok, Id}, State};     
		{error, Reason} ->
		    {reply, {error, Reason}, State}
	    end;
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;
handle_call({update_geom, Id, Geometry}, _From, State) ->
    Reply = node_geom_db:update(Id, Geometry),
    {reply, Reply, State};
handle_call({serialize_geom, Id}, _From, State) ->
    ok = node_geom_db:serialize(Id),
    {reply, ok, State};
handle_call({deserialize_geom, Id}, _From, State) ->
    ok = node_geom_db:deserialize(Id),
    {reply, ok, State};
handle_call({mesh_geom, Id}, _From, State) ->
    Geometry = node_geom_db:geometry(Id),
    Hash = node_geom_db:hash(Id),
    MeshReply = ensure_brep_exists(Id, Geometry, Hash, fun() -> node_mesh_db:mesh(Hash) end),
    {reply, MeshReply, State};
handle_call({serialize_brep, Id}, _From, State) ->
    Hash = node_geom_db:hash(Id),
    Geometry = node_geom_db:geometry(Id),
    Reply = ensure_brep_exists(Id, Geometry, Hash, fun() -> ok end),
    {reply, Reply, State};
handle_call({stl, Id}, _From, State) ->
    Hash = node_geom_db:hash(Id),
    Geometry = node_geom_db:geometry(Id),
    Reply = ensure_brep_exists(Id, Geometry, Hash, fun() -> node_mesh_db:stl(Hash) end),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, unmatched_handle_call_in_gen_server, State}.

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

ensure_brep_exists(Id, Geometry, Hash, TopLevelFn) ->
    node_log:info("Top level BRep exists: ~p~n", [Hash]),
    ChildNodes = get_child_nodes(Geometry),
    case ensure_child_breps_exist(ChildNodes) of
	ok ->
	    case node_brep_db:create(Hash, Geometry) of 
		ok ->
		    Result = TopLevelFn(),
		    %% Purge the top level and children
		    purge_nodes([{Id, Geometry, Hash}|ChildNodes]),
		    Result;
		{error, R1} ->
		    %% Purge the children
		    purge_nodes(ChildNodes),
		    {error, R1}
	    end;
	{error, R2} ->
	    %% Purge the children
	    purge_nodes(ChildNodes),
	    {error, R2}
    end.

ensure_child_breps_exist([]) ->
    ok;
ensure_child_breps_exist([{Id, Geometry, Hash}|Rest]) ->
    node_log:info("Creating BREP for ~p[~p]~n", [Id, Hash]),
    ChildNodes = get_child_nodes(Geometry),
    case ensure_child_breps_exist(ChildNodes) of
	ok ->
	    case node_brep_db:create(Hash, Geometry) of
		{error, R1} ->
		    {error, R1};
		ok ->
		    ensure_child_breps_exist(Rest)
	    end;
	{error, R2} ->
	    {error, R2}
    end.

get_child_nodes(Geometry) ->
    {struct, GeomProps} = Geometry,
    case lists:keyfind(<<"children">>, 1, GeomProps) of
	false ->
	    [];
	{<<"children">>, Children} ->
	    lists:map(fun(ChildIdBin) ->
			      ChildId = binary_to_list(ChildIdBin),
			      ChildGeometry = node_geom_db:geometry(ChildId),
			      ChildHash = node_geom_db:hash(ChildId),
			      {ChildId, ChildGeometry, ChildHash}
		      end,
		      Children)
    end.

purge_nodes(Nodes) ->
    lists:map(fun({_,_,Hash}) ->
		      node_brep_db:purge(Hash)
	      end,
	      Nodes),
    ok.
