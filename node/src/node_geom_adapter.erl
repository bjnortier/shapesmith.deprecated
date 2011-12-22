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
	    ResponseJSON = {[{<<"path">>, iolist_to_binary(Path)},
			     {<<"SHA">>, list_to_binary(SHA)}]},
	    {ok, ResponseJSON};
	{error,worker_timeout} ->
	    {error, 500, {[{<<"error">>, <<"timeout">>}]}};
	{error, Reason} ->
	    lager:error("Geometry create failed: ~p", [Reason]),
	    {error, 500, {[{<<"error">>, <<"internal error">>}]} }
    end.

get(ReqData, User, Design) ->
    Recursive = case wrq:get_qs_value("recursive", "false", ReqData) of
		    "true" -> true;
		    _ -> false
		end,
    case {wrq:path_info(sha, ReqData), Recursive} of
	{undefined, _} ->
	    undefined;
	{SHA, false} ->
	    node_db:get(User, Design, geom, SHA);
	{SHA, true} ->
	    recursive_geom_get(User, Design, SHA)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 private                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

recursive_geom_get(User, Design, SHA) ->
    Geom = node_db:get(User, Design, geom, SHA),
    {Props} = Geom,
    ChildSHAs = case lists:keyfind(<<"children">>, 1, Props) of
		    false -> [];
		    {_, SHAs} -> SHAs
		end,
    RecursiveChildren = lists:map(fun(ChildSHABin) ->
					  ChildSHA = binary_to_list(ChildSHABin),
					  recursive_geom_get(User, Design, ChildSHA)
				  end,
				  ChildSHAs),
    NewProps = lists:keyreplace(<<"children">>, 1, Props, 
				{<<"children">>, RecursiveChildren}),
    {[{<<"sha">>, list_to_binary(SHA)},
      {<<"geometry">>, {NewProps}}]}.


