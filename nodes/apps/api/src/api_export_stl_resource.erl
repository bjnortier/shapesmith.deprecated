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

-module(api_export_stl_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([
         init/1, 
         allowed_methods/2,
	 is_authorized/2,
	 content_types_provided/2,
	 provide_content/2,
	 resource_exists/2
        ]).
-include_lib("webmachine/include/webmachine.hrl").

-record(context, {method, user, design, commit_SHA, commit_JSON }).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    Commit = case string:tokens(wrq:path_info(sha, ReqData), ".") of
		 [C, "stl"] -> C;
		 C -> C
	     end,
    Context1 = Context#context{ method=wrq:method(ReqData),
				user=wrq:path_info(user, ReqData),
				design=wrq:path_info(design, ReqData),
				commit_SHA=Commit},
    {['GET'], ReqData, Context1}.

is_authorized(ReqData, Context = #context{ user = User,
					   design = Design,
					   commit_SHA = CommitSHA}) ->
    case api_db:is_published_stl(User, Design, CommitSHA) of
	true ->
	    {true, ReqData, Context};
	false ->
	    api_resource:forbidden_if_not_authorized(ReqData, Context)
    end.

resource_exists(ReqData, Context = #context{ user = User,
					     design = Design,
					     commit_SHA = CommitSHA}) ->

    case api_db:get(User, Design, commit, CommitSHA) of
	undefined ->
	    {false, ReqData, Context};
	CommitJSON ->
	    {true, ReqData, Context#context{ commit_JSON = CommitJSON }}
    end.

content_types_provided(ReqData, Context) ->
    {[{"application/sla", provide_content}], ReqData, Context}.

provide_content(ReqData, Context = #context{ user = User,
					     design = Design,
					     commit_JSON = CommitJSON} ) ->
    {CommitProps} = CommitJSON,
    {_, Geoms} = lists:keyfind(<<"geoms">>, 1, CommitProps),
    Facets = lists:map(fun(GeomSHABin) ->
			       GeomSHA = binary_to_list(GeomSHABin),
                               case api_master:stl(User, Design, GeomSHA) of
                                   {ok, STL} ->
                                       Lines = string:tokens(binary_to_list(STL), "\n"),
                                       [_|FacetsPlusFooter] = Lines,
                                       [_|ReverseFacets] = lists:reverse(FacetsPlusFooter),
                                       string:join(lists:reverse(ReverseFacets), "\n");
                                   empty ->
                                       ""
                               end
		       end,
		       Geoms),
    ReqData1 =  wrq:set_resp_header("Content-disposition",
				    "attachment; filename=" ++ Design ++ ".stl",
				    ReqData),
    ReqData2 =  wrq:set_resp_header("Content-Type",
				    "application/sla",
				    ReqData1),

    {list_to_binary("solid " ++ Design ++ "\n" ++ string:join(Facets, "\n") ++ "\n" ++ "endsolid " ++ Design),
     ReqData2,
     Context}.

