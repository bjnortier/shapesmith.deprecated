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

-module(node_commit_adapter).
-author('Benjamin Nortier <bjnortier@gmail.com>').

-export([methods/1, validate/4, create/4, get/3]).

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

validate(_ReqData, _User, _Design, {Props}) ->
    case {validate_geoms(Props), validate_parent(Props)} of
    	{ok, ok} ->
    	    ok;
    	{{error, GeomError}, _} ->
    	    {error, {[{<<"validation">>, GeomError}]}};
    	{ok, {error, ParentError}} ->
    	    {error, {[{<<"validation">>, ParentError}]}}
    end;
validate(_ReqData, _User, _Design, _) ->
    {error, {[{<<"validation">>, <<"commit must be an object">>}]}}.

validate_geoms(Props) ->
    case lists:keyfind(<<"geoms">>, 1, Props) of 
	false ->
	    {error, <<"no geoms specified">>};
	{<<"geoms">>, X} when is_list(X) ->
	    ok;
	_ ->
	    {error, <<"geoms must be an array">>}
    end.

validate_parent(Props) ->
    case lists:keyfind(<<"parent">>, 1, Props) of 
	false ->
	    {error, <<"no parent commit specified">>};
	{<<"parent">>, X} when is_binary(X) ->
	    ok;
	_ ->
	    {error, <<"parent commit must be a string">>}
    end.

create(_ReqData, User, Design, RequestJSON) ->
    {ok, SHA} = node_db:create(User, Design, commit, RequestJSON),
    Path = io_lib:format("/~s/~s/commit/~s", [User, Design, SHA]),
    ResponseJSON = {[{<<"path">>, iolist_to_binary(Path)},
		     {<<"SHA">>, list_to_binary(SHA)}]},
    {ok, ResponseJSON}.

get(ReqData, User, Design) ->
    case wrq:path_info(sha, ReqData) of
	undefined ->
	    undefined;
	SHA ->
	    node_db:get(User, Design, commit, SHA)
    end.


