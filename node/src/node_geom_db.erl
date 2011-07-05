-module(node_geom_db).
-export([exists/1, create/1, update/2, geometry/1, recursive_geometry/1, hash/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

exists(Id) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:exists(geom, Id).

raw_geom_record(Id) ->
    gen_server:call(?MODULE, {raw_geom_record, Id}).

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
    DB:get(geom, Id).

recursive_geometry(Id) ->
    {ok, DB} = application:get_env(node, db_module),
    {struct, Props} = DB:get(geom, Id),
    case lists:keyfind(<<"children">>, 1, Props) of
        false ->
            {struct, Props};
        {<<"children">>, ChildIds} ->
            RecursiveChildren = lists:map(fun(ChildIdBin) ->
						  ChildId = binary_to_list(ChildIdBin),
						  recursive_geometry(ChildId)
					  end,
					  ChildIds),
            {struct, lists:keyreplace(<<"children">>, 1, Props, {<<"children">>, RecursiveChildren})}
    end.  

hash(Id) ->
    Geom = recursive_geometry(Id),
    node_hash:hash_geometry(Geom).


                                  
