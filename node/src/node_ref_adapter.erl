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

-module(node_ref_adapter).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([validate/5, update/5, exists/4, get/4]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 public                                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

validate(_User, _Design, "heads", "master", NewCommitRef) when is_binary(NewCommitRef) ->
    ok;
validate(_User, _Design, _RefType, _Ref, NewCommitRef) when is_binary(NewCommitRef) ->
    {error, <<"only \"heads\" and \"master\" supported">>};
validate(_User, _Design, _RefType, _Ref, _NewCommitRef) ->
    {error, <<"string commit SHA expected">>}.


exists(User, Design, RefType, Ref) ->
    case get(User, Design, RefType, Ref) of
	undefined ->
	    false;
	_Commit -> 
	    true
    end.

get(User, Design, RefType, Ref) ->
    {RootProps} = node_db:get_root(User, Design),
    {Refs} =  case lists:keyfind(<<"refs">>, 1, RootProps) of
		  {_, R} -> R;
		  false -> {[]}
	      end,
    {Commits} = case lists:keyfind(list_to_binary(RefType), 1, Refs) of
		  {_, Cs} -> Cs;
		  false -> {[]}
	      end,
    case lists:keyfind(list_to_binary(Ref), 1, Commits) of
	false -> undefined;
	{_, Commit} -> Commit
    end.

update(User, Design, RefType, Ref, NewCommitRef) when is_binary(NewCommitRef) ->
    Root = node_db:get_root(User, Design),
    {RootProps} = Root,
    {Refs} =  case lists:keyfind(<<"refs">>, 1, RootProps) of
		  {_, R} -> R;
		  false -> {[]}
	      end,
    {Commits} = case lists:keyfind(list_to_binary(RefType), 1, Refs) of
		  {_, C} -> C;
		  false -> {[]}
	      end,
    NewCommits = lists:keyreplace(list_to_binary(Ref), 1, Commits, {list_to_binary(Ref), NewCommitRef}),
    NewRefs = lists:keyreplace(list_to_binary(RefType), 1, Refs, {list_to_binary(RefType), {NewCommits}}),
    NewRootProps = lists:keyreplace(<<"refs">>, 1, RootProps, {<<"refs">>, {NewRefs}}),
    ok = node_db:put_root(User, Design, {NewRootProps}),
    lager:info("Updated ~s/~s ~s/~s: ~p", [User, Design, RefType, RefType, jiffy:encode({NewRootProps})]),
    {ok, <<"updated">>}.


-ifdef(TEST).

get_test_() ->
    {setup, 
     fun() -> 
	     meck:new(node_db),
	     meck:expect(node_db, 
			 get_root, 
			 fun("bjnortier", "iphonedock") -> 
				 {[{<<"refs">>, 
				    {[{<<"heads">>,
				       {[{<<"master">>, <<"009c">>}]} 
				      }]} 
				   }]}
			 end)
     end, 
     fun(_) -> 
	     meck:unload(node_db)
     end,
     [
      ?_assertEqual(<<"009c">>, get("bjnortier", "iphonedock", "heads", "master")),
      ?_assert(meck:validate(node_db))
     ]
    }.

update_test_() ->
    {setup, 
     fun() -> 
	     meck:new(node_db),
	     meck:expect(node_db, 
			 get_root, 
			 fun("bjnortier", "iphonedock") -> 
				 <<"{\"refs\":{\"heads\":{\"master\":\"009c\"}}}">>
			 end),
	     meck:expect(node_db, 
			 put_root, 
			 fun("bjnortier", "iphonedock", <<"{\"refs\":{\"heads\":{\"master\":\"af56\"}}}">>) ->
			    ok
			 end)
	     
     end, 
     fun(_) -> 
	     meck:unload(node_db)
     end,
     [
      ?_assertEqual(ok, update("bjnortier", "iphonedock", "heads", "master", <<"af56">>)),
      ?_assert(meck:validate(node_db))
     ]
    }.

-endif.
