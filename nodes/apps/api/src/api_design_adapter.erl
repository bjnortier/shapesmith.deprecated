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

-module(api_design_adapter).
-author('Benjamin Nortier <bjnortier@gmail.com>').

-export([methods/1, validate/4, create/4, get/3, delete/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 public                                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

methods(_ReqData) ->
    ['GET', 'POST', 'DELETE'].

validate(_ReqData, _User, Design, RequestJSON) ->
    case {re:run(Design, "^[a-zA-Z0-9-_]+$"), RequestJSON} of
	{nomatch, _} ->
	    {error, {[{<<"newDesignName">>, 
		       <<"design name can only contain letters, numbers, dashes and underscores">>}]}};
	{{match, _}, {[]}} ->
	    ok;
	_ ->
	    {error, <<"only {} accepted">>}
    end.

create(_ReqData, User, Design, {[]}) ->
    case api_db:get_root(User, Design) of
	undefined ->
	    {ok, CommitSHA} = api_db:create(User, Design, commit, {[{<<"geoms">>, []}]}),
	    Root = {[{<<"refs">>, 
		      {[{<<"heads">>, 
			 {[{<<"master">>, list_to_binary(CommitSHA)}]} 
			}]}
		     }]},
	    ok = api_db:put_root(User, Design, Root),
	    ok = api_db:add_design(User, Design),
	    {ok, Root};
	_ ->
	    {error, 400, {[{<<"newDesignName">>, <<"already exists">>}]}}
    end.

get(_ReqData, User, Design) ->
    api_db:get_root(User, Design).

delete(_ReqData, User, Design) ->
    api_db:delete_root(User, Design),
    api_db:remove_design(User, Design).
