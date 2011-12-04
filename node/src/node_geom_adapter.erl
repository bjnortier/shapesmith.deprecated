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

-module(node_geom_adapter).
-author('Benjamin Nortier <bjnortier@gmail.com>').

-export([methods/1, validate/4, create/4, exists/3, get/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 public                                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

methods(ReqData) ->
    case wrq:path_info(sha, ReqData) of
	undefined -> 
	    ['POST'];
	_ ->
	    ['GET']
    end.

validate(_ReqData, _User, _Design, Geometry) ->
    case node_validation:geom(Geometry) of
	ok ->
	    ok;
	{error, ErrorParams} ->
	    {error, {[{<<"validation">>, ErrorParams}]}}
    end.

create(_ReqData, User, Design, RequestJSON) ->
    case node_master:create_geom(User, Design, RequestJSON) of
	{ok, SHA} ->
	    Path = io_lib:format("/~s/~s/geom/~s", [User, Design, SHA]),
	    ResponseJSON = {[{<<"path">>, iolist_to_binary(Path)}]},
	    {ok, ResponseJSON};
	{error,worker_timeout} ->
	    {error, 500, {[{<<"error">>, <<"timeout">>}]}};
	{error, Reason} ->
	    lager:error("Geometry create failed: ~p", [Reason]),
	    {error, 500, {[{<<"error">>, <<"internal error">>}]} }
    end.

exists(ReqData, User, Design) ->
    case wrq:path_info(sha, ReqData) of
	undefined ->
	    false;
	SHA ->
	    node_db:exists(User, Design, geom, SHA)
    end.

get(ReqData, User, Design) ->
    SHA = wrq:path_info(sha, ReqData),
    node_db:get(User, Design, geom, SHA).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 private                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


