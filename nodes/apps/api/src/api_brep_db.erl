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

-module(api_brep_db).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([is_serialized/1, create/3]).
-export([serialize/2, purge/2, purge_all/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


is_serialized(SHA) ->
    api_db:exists_brep(SHA).

create(WorkerPid, SHA, Geometry) ->
    %% If the SHA BREP exists, use that. Otherwise
    %% create using the worker
    case api_db:get_brep(SHA) of

        undefined ->
            lager:info("Creating BREP for ~p using worker~n", [SHA]),

            {GeomProps} = Geometry,
            {<<"type">>, GeomType} = lists:keyfind(<<"type">>, 1, GeomProps),
	    case create_type(WorkerPid, SHA, GeomType, Geometry) of
		<<"\"ok\"">> ->
		    ok;
		{error, Reason} ->
		    {error, Reason};
		ErrorMsg ->
		    {error, ErrorMsg}
	    end;

        S11N ->

            lager:info("Creating BREP for ~p from file~n", [SHA]),
            Msg = {[{<<"type">>, <<"deserialize">>},
		    {<<"id">>, list_to_binary(SHA)},
		    {<<"s11n">>, S11N}]},
            case api_worker_pool:call(WorkerPid, jiffy:encode(Msg)) of
		<<"\"ok\"">> ->
		    ok;
		{error, Reason} ->
		    {error, Reason};
		ErrorMsg ->
		    {error, ErrorMsg}
	    end
    end.

purge(WorkerPid, SHA) ->
    case api_db:exists_brep(SHA) of
	false ->
	    ok = serialize_to_disk(WorkerPid, SHA);
	_ ->
	    ok
    end,
    Msg = {[{<<"purge">>, list_to_binary(SHA)}]},
    case api_worker_pool:call(WorkerPid, jiffy:encode(Msg)) of
	"true" ->
	    ok;
        "false" ->
            lager:error("Purge for SHA ~p failed: not_found", [SHA]),
            {error, not_found};
	{error, Reason} ->
	    {error, Reason};
	Error -> 
	    {error, Error}
    end.

purge_all(WorkerPid) ->
    Msg = <<"purge_all">>,
    case api_worker_pool:call(WorkerPid, jiffy:encode(Msg)) of
	<<"\"ok\"">> ->
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

serialize_to_disk(WorkerPid, SHA) ->
    Msg = {[{<<"type">>, <<"serialize">>},
	    {<<"id">>, list_to_binary(SHA)}]},
    {[{<<"s11n">>, S11N}]} = jiffy:decode(api_worker_pool:call(WorkerPid, jiffy:encode(Msg))),
    api_db:put_brep(SHA, S11N).
    

create_type(WorkerPid, SHA, <<"union">>, Geometry) ->
    create_boolean(WorkerPid, SHA, <<"union">>, Geometry);
create_type(WorkerPid, SHA, <<"subtract">>, Geometry) ->
    create_boolean(WorkerPid, SHA, <<"subtract">>, Geometry);
create_type(WorkerPid, SHA, <<"intersect">>, Geometry) ->
    create_boolean(WorkerPid, SHA, <<"intersect">>, Geometry);
%% Non-bool pass through
create_type(WorkerPid, SHA, _, Geometry) ->
    worker_create(WorkerPid, SHA, Geometry).

create_boolean(WorkerPid, SHA, Type, Geometry) ->
    {GeomProps} = Geometry,
    {<<"children">>, ChildSHAs} = lists:keyfind(<<"children">>, 1, GeomProps),
    Transforms = case lists:keyfind(<<"transforms">>, 1, GeomProps) of
                     false -> [];
                     {<<"transforms">>, T} -> T
                 end,
    worker_create(WorkerPid, SHA, {[{<<"type">>, Type},
				     {<<"children">>, ChildSHAs},
				     {<<"transforms">>, Transforms}
				    ]}).

worker_create(WorkerPid, SHA, Geometry) ->
    Msg = {[{<<"type">>, <<"create">>},
	    {<<"id">>, list_to_binary(SHA)},
	    {<<"geometry">>, Geometry}
	   ]},
    api_worker_pool:call(WorkerPid, jiffy:encode(Msg)).
