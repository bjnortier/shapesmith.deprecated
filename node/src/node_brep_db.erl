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

-module(node_brep_db).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([is_serialized/1, create/3]).
-export([serialize/2, purge/2, purge_all/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


is_serialized(Hash) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:exists(brep, Hash).

create(WorkerPid, Hash, Geometry) ->
    %% If the serialized BREP exists, use that. Otherwise
    %% create using the worker
    {ok, DB} = application:get_env(node, db_module),
    case DB:exists(brep, Hash) of
        true ->

            node_log:info("Creating BREP for ~p from file~n", [Hash]),
	    S11N = DB:get(brep, Hash),
            %% Insert into worker
            Msg = {struct, [{<<"type">>, <<"deserialize">>},
                            {<<"id">>, list_to_binary(Hash)},
                            {<<"s11n">>, S11N}]},
            case node_worker_pool:call(WorkerPid, mochijson2:encode(Msg)) of
		"\"ok\"" ->
		    ok;
		{error, Reason} ->
		    {error, Reason};
		ErrorMsg ->
		    {error, ErrorMsg}
	    end;

        false ->
            
            node_log:info("Creating BREP for ~p using worker~n", [Hash]),

            {struct, GeomProps} = Geometry,
            {<<"type">>, GeomType} = lists:keyfind(<<"type">>, 1, GeomProps),
	    case create_type(WorkerPid, Hash, GeomType, Geometry) of
		"\"ok\"" ->
		    ok;
		{error, Reason} ->
		    {error, Reason};
		ErrorMsg ->
		    {error, ErrorMsg}
	    end
    end.

purge(WorkerPid, Hash) ->
    {ok, DB} = application:get_env(node, db_module),
    case DB:exists(brep, Hash) of
	false ->
	    ok = serialize_to_disk(WorkerPid, Hash);
	_ ->
	    ok
    end,
    node_log:info("Purging ~p~n", [Hash]),
    Msg = {struct, [{<<"purge">>, list_to_binary(Hash)}]},
    case node_worker_pool:call(WorkerPid, mochijson2:encode(Msg)) of
	"true" ->
	    ok;
	{error, Reason} ->
	    {error, Reason};
	Error -> 
	    {error, Error}
    end.

purge_all(WorkerPid) ->
    Msg = <<"purge_all">>,
    case node_worker_pool:call(WorkerPid, mochijson2:encode(Msg)) of
	"\"ok\"" ->
	    ok;
	{error, Reason} ->
	    {error, Reason};
	Error -> 
	    {error, Error}
    end.

serialize(WorkerPid, Hash) ->
    serialize_to_disk(WorkerPid, Hash).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 private                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

serialize_to_disk(WorkerPid, Hash) ->
    Msg = {struct, [{<<"type">>, <<"serialize">>},
                    {<<"id">>, list_to_binary(Hash)}]},
    {struct, [{<<"s11n">>, S11N}]} = mochijson2:decode(node_worker_pool:call(WorkerPid, mochijson2:encode(Msg))),
    node_log:info("writing brep for ~p~n", [Hash]),
    {ok, DB} = application:get_env(node, db_module),
    DB:put(brep, Hash, S11N).
    

create_type(WorkerPid, Hash, <<"union">>, Geometry) ->
    create_boolean(WorkerPid, Hash, <<"union">>, Geometry);
create_type(WorkerPid, Hash, <<"subtract">>, Geometry) ->
    create_boolean(WorkerPid, Hash, <<"subtract">>, Geometry);
create_type(WorkerPid, Hash, <<"intersect">>, Geometry) ->
    create_boolean(WorkerPid, Hash, <<"intersect">>, Geometry);
%% Non-bool pass through
create_type(WorkerPid, Hash, _, Geometry) ->
    worker_create(WorkerPid, Hash, Geometry).

create_boolean(WorkerPid, Hash, Type, Geometry) ->
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
    worker_create(WorkerPid, Hash, {struct, [{<<"type">>, Type},
					     {<<"children">>, ChildHashes},
					     {<<"transforms">>, Transforms}
					    ]}).
worker_create(WorkerPid, Hash, Geometry) ->
    Msg = {struct, [{<<"type">>, <<"create">>},
                    {<<"id">>, list_to_binary(Hash)},
                    {<<"geometry">>, Geometry}
                   ]},
    node_worker_pool:call(WorkerPid, mochijson2:encode(Msg)).
