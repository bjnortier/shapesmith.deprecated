-module(node_geom_db).
-export([exists/1, create/1, update/2, geometry/1, recursive_geometry/1, hash/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

exists(Id) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:exists(geom, Id).

create(Geometry) ->
    case node_validation:geom(Geometry) of
	{error, ErrorParams} ->
		{error, {validation, ErrorParams}};
	ok ->
	    {ok, DB} = application:get_env(node, db_module),
	    Id = DB:create(geom, Geometry),
	    {ok, Id}
    end.

update(Id, Geometry) ->
    case node_validation:geom(Geometry) of
        {error, ErrorParams} ->
            {error, {validation, ErrorParams}};
        ok ->
	    {ok, DB} = application:get_env(node, db_module),
	    DB:put(geom, Id, Geometry)
    end.

geometry(Id) ->
    {ok, DB} = application:get_env(node, db_module),
    {struct, Props1} = DB:get(geom, Id),
    {struct, [{<<"id">>, list_to_binary(Id)}|Props1]}.

recursive_geometry(Id) ->
    {ok, DB} = application:get_env(node, db_module),
    {struct, Props1} = DB:get(geom, Id),
    Props2 = [{<<"id">>, list_to_binary(Id)}|Props1],
    case lists:keyfind(<<"children">>, 1, Props2) of
        false ->
            {struct, Props2};
        {<<"children">>, ChildIds} ->
            RecursiveChildren = lists:map(fun(ChildIdBin) ->
						  ChildId = binary_to_list(ChildIdBin),
						  recursive_geometry(ChildId)
					  end,
					  ChildIds),
            {struct, lists:keyreplace(<<"children">>, 1, Props2, {<<"children">>, RecursiveChildren})}
    end.  

hash(Id) ->
    Geom = recursive_geometry(Id),
    node_hash:hash_geometry(Geom).


                                  
