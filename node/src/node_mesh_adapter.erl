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

-module(node_mesh_adapter).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([
	 methods/1, 
	 get/3
        ]).

methods(_ReqData) ->
    ['GET'].

get(ReqData, User, Design) ->
    SHA = wrq:path_info(sha, ReqData),
    case node_master:mesh_geom(User, Design, SHA) of
	geometry_doesnt_exist ->
	    undefined;
	{ok, Mesh} ->
	    Mesh;
	{error, Err} ->
	    lager:error("Meshing failed: ~p~n", [Err]),
	    Error= {[{<<"error">>, <<"Could not mesh geometry">>}]},
	    {error, Error}
    end.

