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

-module(api_db).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([exists/4, create/4, get/4]).
-export([exists_root/2, put_root/3, get_root/2, delete_root/2]).
-export([create_user/3, validate_password/2]).
-export([get_designs/1, add_design/2, remove_design/2]).
-export([get_brep/1, exists_brep/1, put_brep/2]).
-export([publish_stl/3, is_published_stl/3]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

exists(User, Design, Type, SHA) ->
    {ok, DB} = application:get_env(api, db_module),
    DB:exists(bucket(User, Design), key(Type, SHA)).

create(User, Design, Type, JSON) ->
    {ok, DB} = application:get_env(api, db_module),
    SHA = api_hash:hash_json(JSON),
    ok = DB:put(bucket(User, Design), key(Type, SHA), jiffy:encode(JSON)),
    {ok, SHA}.

get(User, Design, Type, SHA) ->
    {ok, DB} = application:get_env(api, db_module),
    case DB:get(bucket(User, Design), key(Type, SHA)) of
	undefined ->
	    undefined;
	EncodedJSON ->
	    jiffy:decode(EncodedJSON)
    end.

exists_root(User, Design) ->
    {ok, DB} = application:get_env(api, db_module),
    DB:exists(bucket(User, Design), <<"_root">>).

get_root(User, Design) ->
    {ok, DB} = application:get_env(api, db_module),
    case DB:get(bucket(User, Design), <<"_root">>) of
	undefined ->
	    undefined;
	EncodedJSON ->
	    jiffy:decode(EncodedJSON)
    end.

put_root(User, Design, JSON) ->
    {ok, DB} = application:get_env(api, db_module),
    DB:put(bucket(User, Design), <<"_root">>, jiffy:encode(JSON)).

delete_root(User, Design) ->
    {ok, DB} = application:get_env(api, db_module),
    DB:delete(bucket(User, Design), <<"_root">>).


create_user(User, Password, EmailAddress) ->
    {ok, DB} = application:get_env(api, db_module),
    case DB:exists(bucket(User), <<"_user">>) of
    	false ->
	    {ok, Salt} = api_bcrypt:gen_salt(),
	    {ok, Hash} = api_bcrypt:hashpw(Password, Salt),
	    Props1 = [{<<"password[bcrypt]">>, list_to_binary(Hash)}],
	    Props2 = case EmailAddress of
			 "" -> 
			     Props1;
			 _ ->
			     [{<<"email-address">>, list_to_binary(EmailAddress)}|Props1]
		     end,

    	    DB:put(bucket(User), <<"_user">>, jiffy:encode({Props2})),
    	    ok;
    	true ->
    	    already_exists
    end.

validate_password(User, Password) ->
    {ok, DB} = application:get_env(api, db_module),
    case DB:get(bucket(User), <<"_user">>) of
	undefined ->
	    user_doesnt_exist;
	UserJSON ->
	    {Props} = jiffy:decode(UserJSON),
	    {_, BCryptHashBin} = lists:keyfind(<<"password[bcrypt]">>, 1, Props),
	    BCryptHash = binary_to_list(BCryptHashBin),
	    case api_bcrypt:hashpw(Password, BCryptHash) of
		{ok, BCryptHash} ->
		    ok;
		_ ->
		    invalid_password
	    end
    end.

get_designs(User) ->
    {ok, DB} = application:get_env(api, db_module),
    case DB:get(bucket(User), <<"_designs">>) of
	undefined ->
	    [];
	EncodedJSON ->
	    jiffy:decode(EncodedJSON)
    end.

add_design(User, Design) ->
    {ok, DB} = application:get_env(api, db_module),
    ExistingDesigns = case DB:get(bucket(User), <<"_designs">>) of
			  undefined ->
			      [];
			  EncodedJSON ->
			     jiffy:decode(EncodedJSON)
		      end,
    NewDesigns = ExistingDesigns ++ [list_to_binary(Design)],
    DB:put(bucket(User), <<"_designs">>, jiffy:encode(NewDesigns)).

remove_design(User, Design) ->
    {ok, DB} = application:get_env(api, db_module),
    ExistingDesigns = case DB:get(bucket(User), <<"_designs">>) of
			  undefined ->
			      [];
			  EncodedJSON ->
			     jiffy:decode(EncodedJSON)
		      end,
    RemainingDesigns = lists:delete(list_to_binary(Design), ExistingDesigns),
    DB:put(bucket(User), <<"_designs">>, jiffy:encode(RemainingDesigns)).

publish_stl(User, Design, CommitSHA) ->
    {ok, DB} = application:get_env(api, db_module),
    Key = <<"_stls_published">>, 
    case DB:get(bucket(User, Design), Key) of
	undefined ->
	    DB:put(bucket(User, Design), Key, jiffy:encode([list_to_binary(CommitSHA)]));
	SHAsJSON ->
	    SHAs = jiffy:decode(SHAsJSON),
            case lists:member(list_to_binary(CommitSHA), SHAs) of
                true ->
                    ok;
                false ->
                    DB:put(bucket(User, Design), Key, jiffy:encode([list_to_binary(CommitSHA)|SHAs]))
            end
    end.

is_published_stl(User, Design, CommitSHA) ->
    {ok, DB} = application:get_env(api, db_module),	    
    Key = <<"_stls_published">>, 
    case DB:get(bucket(User, Design), Key) of
	undefined ->
	    false;
	PublishedSHAsJSON ->
	    lists:member(list_to_binary(CommitSHA), jiffy:decode(PublishedSHAsJSON))
    end.

exists_brep(SHA) when is_list(SHA) ->
    {ok, DB} = application:get_env(api, db_module),
    DB:exists(<<"_brep">>, list_to_binary(SHA)).

get_brep(SHA) when is_list(SHA) ->
    {ok, DB} = application:get_env(api, db_module),
    case DB:get(<<"_brep">>, list_to_binary(SHA)) of
	undefined ->
	    undefined;
	BRep ->
	    jiffy:decode(BRep)
    end.

put_brep(SHA, BRep) when is_list(SHA) ->
    {ok, DB} = application:get_env(api, db_module),
    DB:put(<<"_brep">>, list_to_binary(SHA), jiffy:encode(BRep)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Private API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

key(vertex, SHA) ->
    list_to_binary("vertex/" ++ SHA);
key(graph, SHA) ->
    list_to_binary("graph/" ++ SHA);
key(geom, SHA) ->
    list_to_binary("geom/" ++ SHA);
key(commit, SHA) ->
    list_to_binary("commit/" ++ SHA).

bucket(User, Design) ->
    list_to_binary(User ++ "/" ++ Design).

bucket(User) ->
    list_to_binary(User).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              UNIT TESTS                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
 

-ifdef(TEST).

user_exists_test_() ->
    {setup, 
     fun() -> 
	     meck:new(application, [unstick]),
	     meck:new(db),
	     meck:expect(application, get_env, fun(api, db_module) -> {ok, db} end),
	     meck:expect(db, exists, fun(<<"bjnortier">>, <<"_user">>) -> true end)
     end, 
     fun(_) -> 
	     meck:unload(db),
	     meck:unload(application)
     end,
     [
      ?_assertEqual(already_exists, create_user("bjnortier", "123", "a@b.com")),
      ?_assert(meck:validate(application)),
      ?_assert(meck:validate(db))
     ]
    }.

create_user_with_email_test_() ->
    {setup, 
     fun() -> 
	     meck:new(application, [unstick]),
	     meck:new(api_bcrypt),
	     meck:new(db),

	     meck:expect(application, get_env, fun(api, db_module) -> {ok, db} end),
	     meck:expect(db, exists, fun(<<"bjnortier">>, <<"_user">>) -> false end),
	     meck:expect(api_bcrypt, gen_salt, fun() -> {ok, "salt"} end),
	     meck:expect(api_bcrypt, hashpw, fun("password", "salt") -> {ok, "hash"} end),
	     meck:expect(db, put, fun(<<"bjnortier">>, <<"_user">>, 
				      <<"{\"email-address\":\"a@b.com\",\"password[bcrypt]\":\"hash\"}">>) ->
					  ok 
				  end)
     end, 
     fun(_) -> 
	     meck:unload(api_bcrypt),
	     meck:unload(db),
	     meck:unload(application)
     end,
     [
      ?_assertEqual(ok, create_user("bjnortier", "password", "a@b.com")),
      ?_assert(meck:validate(application)),
      ?_assert(meck:validate(db)),
      ?_assert(meck:validate(api_bcrypt))
     ]
    }.


create_user_without_email_test_() ->
    {setup, 
     fun() -> 
	     meck:new(application, [unstick]),
	     meck:new(api_bcrypt),
	     meck:new(db),

	     meck:expect(application, get_env, fun(api, db_module) -> {ok, db} end),
	     meck:expect(db, exists, fun(<<"bjnortier">>, <<"_user">>) -> false end),
	     meck:expect(api_bcrypt, gen_salt, fun() -> {ok, "salt"} end),
	     meck:expect(api_bcrypt, hashpw, fun("password", "salt") -> {ok, "hash"} end),
	     meck:expect(db, put, fun(<<"bjnortier">>, <<"_user">>, 
				      <<"{\"password[bcrypt]\":\"hash\"}">>) ->
					  ok 
				  end)
     end, 
     fun(_) -> 
	     meck:unload(api_bcrypt),
	     meck:unload(db),
	     meck:unload(application)
     end,
     [
      ?_assertEqual(ok, create_user("bjnortier", "password", "")),
      ?_assert(meck:validate(application)),
      ?_assert(meck:validate(db)),
      ?_assert(meck:validate(api_bcrypt))
     ]
    }.


validate_password_test_() ->
    {setup, 
     fun() -> 
	     meck:new(application, [unstick]),
	     meck:new(db),
	     meck:new(api_bcrypt),

	     meck:expect(application, get_env, fun(api, db_module) -> {ok, db} end),
	     meck:expect(db, get, fun(<<"bjnortier">>, <<"_user">>) -> 
					  <<"{\"password[bcrypt]\":\"hash\"}">>;
				     (<<"foo">>, <<"_user">>) ->
					  undefined
				  end),

	     meck:expect(api_bcrypt, hashpw, fun("password", "hash") -> 
						      {ok, "hash"} ;
						 ("wrong_password", "hash") ->
						      {ok, "rubbish"}
					      end)
     end, 
     fun(_) -> 
	     meck:unload(db),
	     meck:unload(api_bcrypt),
	     meck:unload(application)
     end,
     [
      ?_assertEqual(ok, validate_password("bjnortier", "password")),
      ?_assertEqual(user_doesnt_exist, validate_password("foo", "password")),
      ?_assertEqual(invalid_password, validate_password("bjnortier", "wrong_password")),
      ?_assert(meck:validate(application)),
      ?_assert(meck:validate(db)),
      ?_assert(meck:validate(api_bcrypt))
     ]
    }.

-endif.

                                  
