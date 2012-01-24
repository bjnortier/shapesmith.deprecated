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

-module(node_mesh_db).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([mesh/2, stl/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

mesh(WorkerPid, Hash) ->
    case node_worker_pool:call(
	   WorkerPid,
	   jiffy:encode(
	     {[{<<"tesselate">>, list_to_binary(Hash)}]})) of

	{error, Reason} ->
	    {error, Reason};
	JSON ->
	    case jiffy:decode(JSON) of
		{[{<<"error">>, E}]} ->
		    {error, E};
		Result ->
		    {ok, Result}
	    end
    end.

stl(WorkerPid, Hash) ->
    Filename = filename:join(["/tmp", Hash ++ ".stl"]),
    Msg = {[{<<"type">>, <<"stl">>},
	    {<<"id">>, list_to_binary(Hash)},
	    {<<"filename">>, list_to_binary(Filename)}
	   ]},
    case node_worker_pool:call(WorkerPid, jiffy:encode(Msg)) of
	<<"\"ok\"">> ->
	    Result = file:read_file(Filename),
            file:delete(Filename),
            Result;
        <<"\"empty\"">> ->
            empty;
	{error, Reason} ->
	    {error, Reason}
    end.
