-module(node_master).
-export([create_geom/1, update_geom/2, mesh_geom/1, exists/1, geometry/1, recursive_geometry/1, stl/1]).
-export([serialize_geom/1, deserialize_geom/1]).
-export([serialize_brep/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

exists(Id) ->
    node_geom_db:exists(Id).

geometry(Id) ->
    node_geom_db:geometry(Id).

recursive_geometry(Id) ->
    node_geom_db:recursive_geometry(Id).

create_geom(Geometry) ->
    case node_geom_db:create(Geometry) of
	{ok, Id} ->
	    Hash = node_geom_db:hash(Id),
	    case ensure_brep_exists(Id, Geometry, Hash, fun(_WorkerPid) -> ok end) of
		ok ->
		    {ok, Id};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

update_geom(Id, Geometry) ->
    node_geom_db:update(Id, Geometry).

serialize_geom(Id) ->
    ok = node_geom_db:serialize(Id).

deserialize_geom(Id) ->
    ok = node_geom_db:deserialize(Id).

mesh_geom(Id) ->
    Geometry = node_geom_db:geometry(Id),
    Hash = node_geom_db:hash(Id),
    ensure_brep_exists(Id, Geometry, Hash, fun(WorkerPid) -> 
						   node_mesh_db:mesh(WorkerPid, Hash) 
					   end).

serialize_brep(Id) ->
    Hash = node_geom_db:hash(Id),
    Geometry = node_geom_db:geometry(Id),
    ensure_brep_exists(Id, Geometry, Hash, fun(_WorkerPid) -> ok end).

stl(Id) ->
    Hash = node_geom_db:hash(Id),
    Geometry = node_geom_db:geometry(Id),
    ensure_brep_exists(Id, Geometry, Hash, fun(WorkerPid) -> 
						   node_mesh_db:stl(WorkerPid, Hash) 
					   end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 private                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

ensure_brep_exists(Id, Geometry, Hash, TopLevelFn) ->
    case node_worker_pool:get_worker() of
	{error, Error} ->
	    {error, Error};
	WorkerPid ->
	    Result = ensure_child_breps_exist(WorkerPid, [{Id, Geometry, Hash}], TopLevelFn),
	    %% Some brep may be left over if error occured and cleanup wasn't complete
	    node_brep_db:purge_all(WorkerPid),
	    node_worker_pool:return_worker(WorkerPid),
	    Result
	    
    end.

ensure_child_breps_exist(_, [], _) ->
    ok;
ensure_child_breps_exist(WorkerPid, [{Id, Geometry, Hash}|Rest], NodeFn) ->
    node_log:info("Ensure child BRep exists ~p[~p]~n", [Id, Hash]),
    
    ChildNodes = case node_brep_db:is_serialized(Hash) of
		     true ->
			 [];
		     false ->
			 %% We need to create the children
			 get_child_nodes(Geometry)
		 end,
    case ensure_child_breps_exist(WorkerPid, ChildNodes, fun(_WorkerPid) -> ok end) of
	ok ->
	    case node_brep_db:create(WorkerPid, Hash, Geometry) of
		ok ->
		    Result = NodeFn(WorkerPid),
		    purge_nodes(WorkerPid, ChildNodes),
		    ensure_child_breps_exist(WorkerPid, Rest, NodeFn),
		    Result;

		{error, R1} ->
		    purge_nodes(WorkerPid, ChildNodes),
		    {error, R1}
	    end;
	{error, R2} ->
	    purge_nodes(WorkerPid, ChildNodes),
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

purge_nodes(WorkerPid, Nodes) ->
    lists:map(fun({_,_,Hash}) ->
		      node_brep_db:purge(WorkerPid, Hash)
	      end,
	      Nodes),
    ok.
