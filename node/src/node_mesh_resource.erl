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

-module(node_mesh_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([
         init/1, 
         allowed_methods/2,
         resource_exists/2,
	 content_types_provided/2,
	 provide_content/2
        ]).


-include_lib("webmachine/include/webmachine.hrl").

-record(context, {id}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['GET'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    Id = wrq:path_info(id, ReqData),
    Exists = node_master:exists(Id),
    {Exists, ReqData, Context#context{id = Id}}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    Id = Context#context.id,
    case node_master:mesh_geom(Id) of
	{ok, Mesh} ->
	    {mochijson2:encode(Mesh), ReqData, Context};
	{error, Err} ->
	    node_log:error("Meshing failed: ~p~n", [Err]),
	    ErrorJSON = mochijson2:encode({struct, [{<<"error">>, <<"Could not mesh geometry">>}]}),
	    {{halt, 500}, wrq:set_resp_body(ErrorJSON, ReqData), Context}
    end.

