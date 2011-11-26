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

-module(node_geom_db).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([exists/1, create/1, geometry/1, recursive_geometry/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

exists(Sha) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:exists(geom, Sha).

create(Geometry) ->
    case node_validation:geom(Geometry) of
	{error, ErrorParams} ->
		{error, {validation, ErrorParams}};
	ok ->
	    {ok, DB} = application:get_env(node, db_module),
	    SHA = node_hash:hash_json(Geometry),
	    ok = DB:put(geom, SHA, jiffy:encode(Geometry)),
	    {ok, SHA}
    end.

geometry(Sha) ->
    {ok, DB} = application:get_env(node, db_module),
    jiffy:decode(DB:get(geom, Sha)).

recursive_geometry(Id) ->
    {ok, DB} = application:get_env(node, db_module),
    {Props1} = DB:get(geom, Id),
    Props2 = [{<<"id">>, list_to_binary(Id)}|Props1],
    case lists:keyfind(<<"children">>, 1, Props2) of
        false ->
            {Props2};
        {<<"children">>, ChildIds} ->
            RecursiveChildren = lists:map(fun(ChildIdBin) ->
						  ChildId = binary_to_list(ChildIdBin),
						  recursive_geometry(ChildId)
					  end,
					  ChildIds),
            {lists:keyreplace(<<"children">>, 1, Props2, {<<"children">>, RecursiveChildren})}
    end.  


                                  
