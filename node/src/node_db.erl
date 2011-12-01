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

-module(node_db).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([exists/4, create/4, get/4, exists_root/2, put_root/3, get_root/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

exists(User, Design, Type, SHA) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:exists(bucket(User, Design), key(Type, SHA)).

create(User, Design, Type, JSON) ->
    {ok, DB} = application:get_env(node, db_module),
    SHA = node_hash:hash_json(JSON),
    ok = DB:put(bucket(User, Design), key(Type, SHA), jiffy:encode(JSON)),
    {ok, SHA}.

get(User, Design, Type, SHA) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:get(bucket(User, Design), key(Type, SHA)).


exists_root(User, Design) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:exists(bucket(User, Design), "_root").

get_root(User, Design) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:get(bucket(User, Design), "_root").

put_root(User, Design, JSON) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:put(bucket(User, Design), "_root", jiffy:encode(JSON)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Private API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

key(geom, SHA) ->
    "geom/" ++ SHA;
key(commit, SHA) ->
    "commit/" ++ SHA.

bucket(User, Design) ->
    User ++ "/" ++ Design.
    
    

                                  
