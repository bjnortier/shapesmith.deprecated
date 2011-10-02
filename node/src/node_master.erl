%% -*- mode: erlang -*-
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% Copyright 2011 Benjamin Nortier
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.

-module(node_master).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([create_geom/1, update_geom/2, mesh_geom/1, exists/1, geometry/1, recursive_geometry/1, stl/1]).
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
		    node_log:error("create_geom failed: ~p~n", [Reason]),
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

update_geom(Id, Geometry) ->
    node_geom_db:update(Id, Geometry).

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
	    Result = try 
			 ensure_child_breps_exist(WorkerPid, [{Id, Geometry, Hash}], TopLevelFn)
		     catch
			 Type:Exception ->
			     node_log:error("Exception on ensure_child_breps_exist: ~p:~p~n", [Type, Exception]),
			     {error, {Type, Exception}}
		     end,
	    %% Purge the top level geometry. Also,
	    %% Some brep may be left over if error occured and cleanup wasn't complete
	    try 
		node_brep_db:purge(WorkerPid, Hash),
		node_brep_db:purge_all(WorkerPid)
	    catch
		A:B ->
		    node_log:error("Exception on purge_all in ensure_child_breps_exist: ~p:~p~n", [A, B])
	    end,
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
