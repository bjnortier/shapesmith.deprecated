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

-module(api_master).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([create_geom/3, mesh_geom/3, create_and_mesh_geom/3, stl/3]).
-export([ensure_child_breps_exist/5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

create_geom(User, Design, Geometry) ->
    case api_db:create(User, Design, geom, Geometry) of
	{ok, SHA} ->
	    case ensure_brep_exists(User, Design, SHA, Geometry, fun(_WorkerPid) -> ok end) of
		ok ->
		    {ok, SHA};
		{error, Reason} ->
		    lager:error("create_geom failed: ~p~n", [Reason]),
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

mesh_geom(User, Design, SHA) ->
    case api_db:get(User, Design, geom, SHA) of
	undefined ->
	    geometry_doesnt_exist;
	Geometry ->
	    ensure_brep_exists(User, Design, SHA, Geometry, 
			       fun(WorkerPid) -> 
				       api_mesh_db:mesh(WorkerPid, SHA) 
			       end)
    end.

create_and_mesh_geom(User, Design, Geometry) ->
    case api_db:create(User, Design, geom, Geometry) of
	{ok, SHA} ->
	    case ensure_brep_exists(User, Design, SHA, Geometry, 
				    fun(WorkerPid) -> 
					    api_mesh_db:mesh(WorkerPid, SHA)  
				    end) of

		{ok, Mesh} ->
		    {ok, SHA, Mesh};
		{error, Reason} ->
		    lager:error("create_and_mesh_geom failed: ~p~n", [Reason]),
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.
	    
stl(User, Design, SHA) ->
    Geometry = api_db:get(User, Design, geom, SHA),
    ensure_brep_exists(User, Design, SHA, Geometry, 
		       fun(WorkerPid) -> 
			       api_mesh_db:stl(WorkerPid, SHA) 
		       end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 private                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

ensure_brep_exists(User, Design, SHA, Geometry, TopLevelFn) ->
    case api_worker_pool:get_worker() of
	{error, Error} ->
	    {error, Error};
	WorkerPid ->
	    Result = try_with_logging(api_master, ensure_child_breps_exist, 
				      [User, Design, WorkerPid, [{SHA, Geometry}], TopLevelFn]),
	    %% Purge the top level geometry. Also,
	    %% Some brep may be left over if error occured and cleanup wasn't complete
	    try_with_logging(api_brep_db, purge, [WorkerPid, SHA]),
	    try_with_logging(api_brep_db, purge_all, [WorkerPid]),
	    api_worker_pool:return_worker(WorkerPid),
	    Result
	    
    end.

    
try_with_logging(Mod, Fn, Args) ->
    try 
	apply(Mod, Fn, Args)
    catch
	Type:Exception ->
	    lager:error("Exception on ~p:~p(~p) in ensure_child_breps_exist: ~p:~p~n",
			[Mod, Fn, Args, Type, Exception]),
	    {error, {Type, Exception}}
    end.
    

ensure_child_breps_exist(_, _, _, [], _) ->
    ok;
ensure_child_breps_exist(User, Design, WorkerPid, [{SHA, Geometry}|Rest], NodeFn) ->
    ChildNodes = case api_brep_db:is_serialized(SHA) of
		     true ->
			 [];
		     false ->
			 %% We need to create the children
			 get_child_nodes(User, Design, Geometry)
			 
		 end,
    case ensure_child_breps_exist(User, Design, WorkerPid, ChildNodes, 
				  fun(_WorkerPid) -> ok end) of
	ok ->
	    case api_brep_db:create(WorkerPid, SHA, Geometry) of
		ok ->
		    Result = NodeFn(WorkerPid),
		    purge_nodes(WorkerPid, ChildNodes),
		    ensure_child_breps_exist(User, Design, WorkerPid, Rest, NodeFn),
		    Result;

		{error, R1} ->
		    purge_nodes(WorkerPid, ChildNodes),
		    {error, R1}
	    end;
	{error, R2} ->
	    purge_nodes(WorkerPid, ChildNodes),
	    {error, R2}
    end.
    

get_child_nodes(User, Design, Geometry) ->
    {GeomProps} = Geometry,
    case lists:keyfind(<<"children">>, 1, GeomProps) of
	false ->
	    [];
	{<<"children">>, Children} ->
	    lists:map(fun(ChildSHABin) ->
			      ChildSHA = binary_to_list(ChildSHABin),
			      ChildGeometry = api_db:get(User, Design, geom, ChildSHA),
			      {ChildSHA, ChildGeometry}
		      end,
		      Children)
    end.

purge_nodes(WorkerPid, Nodes) ->
    lists:map(fun({SHA,_}) ->
		      api_brep_db:purge(WorkerPid, SHA)
	      end,
	      Nodes),
    ok.
