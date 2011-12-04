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
-export([create_geom/3, mesh_geom/1, stl/1]).
-export([serialize_brep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

recursive_geometry(Sha) ->
    node_geom_db:recursive_geometry(Sha).

create_geom(User, Design, Geometry) ->
    case node_db:create(User, Design, geom, Geometry) of
	{ok, SHA} ->
	    case ensure_brep_exists(SHA, Geometry, fun(_WorkerPid) -> ok end) of
		ok ->
		    {ok, SHA};
		{error, Reason} ->
		    lager:error("create_geom failed: ~p~n", [Reason]),
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

mesh_geom(Sha) ->
    Geometry = node_geom_db:geometry(Sha),
    ensure_brep_exists(Sha, Geometry, fun(WorkerPid) -> 
					      node_mesh_db:mesh(WorkerPid, Sha) 
				      end).

serialize_brep(Sha) ->
    Geometry = node_geom_db:geometry(Sha),
    ensure_brep_exists(Sha, Geometry, fun(_WorkerPid) -> ok end).

stl(Sha) ->
    Geometry = node_geom_db:geometry(Sha),
    ensure_brep_exists(Sha, Geometry, fun(WorkerPid) -> 
					      node_mesh_db:stl(WorkerPid, Sha) 
				      end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 private                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

ensure_brep_exists(Sha, Geometry, TopLevelFn) ->
    case node_worker_pool:get_worker() of
	{error, Error} ->
	    {error, Error};
	WorkerPid ->
	    Result = try 
			 ensure_child_breps_exist(WorkerPid, [{Sha, Geometry}], TopLevelFn)
		     catch
			 Type:Exception ->
			     node_log:error("Exception on ensure_child_breps_exist: ~p:~p~n", [Type, Exception]),
			     {error, {Type, Exception}}
		     end,
	    %% Purge the top level geometry. Also,
	    %% Some brep may be left over if error occured and cleanup wasn't complete
	    try_with_logging(node_brep_db, purge, [WorkerPid, Sha]),
	    try_with_logging(node_brep_db, purge_all, [WorkerPid]),
	    node_worker_pool:return_worker(WorkerPid),
	    Result
	    
    end.

try_with_logging(Mod, Fn, Args) ->
    try 
	apply(Mod, Fn, Args)
    catch
	a:b ->
%Err:Reason ->
	    node_log:error("Exception on ~p:~p(~p) in ensure_child_breps_exist: ~p:~p~n", [Mod, Fn, Args, a, b])
    end.
    

ensure_child_breps_exist(_, [], _) ->
    ok;
ensure_child_breps_exist(WorkerPid, [{Sha, Geometry}|Rest], NodeFn) ->
    node_log:info("Ensure child BRep exists ~p~n", [Sha]),
    
    ChildNodes = case node_brep_db:is_serialized(Sha) of
		     true ->
			 [];
		     false ->
			 %% We need to create the children
			 get_child_nodes(Geometry)
		 end,
    case ensure_child_breps_exist(WorkerPid, ChildNodes, fun(_WorkerPid) -> ok end) of
	ok ->
	    case node_brep_db:create(WorkerPid, Sha, Geometry) of
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
    {GeomProps} = Geometry,
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
