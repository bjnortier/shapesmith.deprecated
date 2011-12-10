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
-export([exists/4, create/4, get/4]).
-export([exists_root/2, put_root/3, get_root/2, delete_root/2]).
-export([get_designs/1, add_design/2, remove_design/2]).
-export([get_brep/1, exists_brep/1, put_brep/2]).

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
    case DB:get(bucket(User, Design), key(Type, SHA)) of
	undefined ->
	    undefined;
	EncodedJSON ->
	    jiffy:decode(EncodedJSON)
    end.


exists_root(User, Design) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:exists(bucket(User, Design), <<"_root">>).

get_root(User, Design) ->
    {ok, DB} = application:get_env(node, db_module),
    case DB:get(bucket(User, Design), <<"_root">>) of
	undefined ->
	    undefined;
	EncodedJSON ->
	    jiffy:decode(EncodedJSON)
    end.

put_root(User, Design, JSON) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:put(bucket(User, Design), <<"_root">>, jiffy:encode(JSON)).

delete_root(User, Design) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:delete(bucket(User, Design), <<"_root">>).

get_designs(User) ->
    {ok, DB} = application:get_env(node, db_module),
    case DB:get(bucket(User), <<"_designs">>) of
	undefined ->
	    [];
	EncodedJSON ->
	    jiffy:decode(EncodedJSON)
    end.

add_design(User, Design) ->
    {ok, DB} = application:get_env(node, db_module),
    ExistingDesigns = case DB:get(bucket(User), <<"_designs">>) of
			  undefined ->
			      [];
			  EncodedJSON ->
			     jiffy:decode(EncodedJSON)
		      end,
    NewDesigns = ExistingDesigns ++ [list_to_binary(Design)],
    DB:put(bucket(User), <<"_designs">>, jiffy:encode(NewDesigns)).

remove_design(User, Design) ->
    {ok, DB} = application:get_env(node, db_module),
    ExistingDesigns = case DB:get(bucket(User), <<"_designs">>) of
			  undefined ->
			      [];
			  EncodedJSON ->
			     jiffy:decode(EncodedJSON)
		      end,
    RemainingDesigns = lists:delete(list_to_binary(Design), ExistingDesigns),
    DB:put(bucket(User), <<"_designs">>, jiffy:encode(RemainingDesigns)).

exists_brep(SHA) when is_list(SHA) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:exists(<<"_brep">>, list_to_binary(SHA)).

get_brep(SHA) when is_list(SHA) ->
    {ok, DB} = application:get_env(node, db_module),
    case DB:get(<<"_brep">>, list_to_binary(SHA)) of
	undefined ->
	    undefined;
	BRep ->
	    BRep
    end.

put_brep(SHA, BRep) when is_list(SHA) andalso is_binary(BRep) ->
    {ok, DB} = application:get_env(node, db_module),
    DB:put(<<"_brep">>, list_to_binary(SHA), BRep).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Private API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

key(geom, SHA) ->
    list_to_binary("geom/" ++ SHA);
key(commit, SHA) ->
    list_to_binary("commit/" ++ SHA).

bucket(User, Design) ->
    list_to_binary(User ++ "/" ++ Design).

bucket(User) ->
    list_to_binary(User).

    
    

                                  
