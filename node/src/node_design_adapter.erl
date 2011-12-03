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

-module(node_design_adapter).
-author('Benjamin Nortier <bjnortier@gmail.com>').

-export([methods/0, validate/3, create/3, get/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 public                                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

methods() ->
    ['GET', 'POST'].

validate(_User, _Design, RequestJSON) ->
    case RequestJSON of
	{[]} ->
	    ok;
	_ ->
	    {error, <<"only {} accepted">>}
    end.

create(User, Design, {[]}) ->
    JSON = {[{<<"children">>, []}]},
    {ok, CommitSHA} = node_db:create(User, Design, commit, JSON),
    
    Root = {[{<<"refs">>, 
	      {[{<<"heads">>, 
		 {[{<<"master">>, list_to_binary(CommitSHA)}]} 
		}]}
	     }]},
    ok = node_db:put_root(User, Design, Root),
    {ok, Root}.

get(User, Design) ->
    node_db:get_root(User, Design).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 private                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

