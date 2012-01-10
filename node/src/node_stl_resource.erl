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

-module(node_stl_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([
         init/1, 
         allowed_methods/2,
	 content_types_provided/2,
	 provide_content/2,
	 resource_exists/2
        ]).
-include_lib("webmachine/include/webmachine.hrl").

-record(context, {user, design, commit_JSON}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['GET'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    SHA = wrq:path_info(sha, ReqData),
    User = wrq:path_info(user, ReqData),
    Design = wrq:path_info(design, ReqData),

    case node_db:get(User, Design, commit, SHA) of
	undefined ->
	    {false, ReqData, Context};
	CommitJSON ->
	    {true, ReqData, Context#context{ user = User, 
					     design = Design,
					     commit_JSON = CommitJSON }}
    end.

content_types_provided(ReqData, Context) ->
    {[{"application/sla", provide_content}], ReqData, Context}.

provide_content(ReqData, Context = #context{ user = User,
					     design = Design,
					     commit_JSON = CommitJSON } ) ->
    {CommitProps} = CommitJSON,
    {_, Geoms} = lists:keyfind(<<"geoms">>, 1, CommitProps),
    Facets = lists:map(fun(GeomSHABin) ->
			       GeomSHA = binary_to_list(GeomSHABin),
			       {ok, STL} = node_master:stl(User, Design, GeomSHA),
			       Lines = string:tokens(binary_to_list(STL), "\n"),
			       [_|FacetsPlusFooter] = Lines,
			       [_|ReverseFacets] = lists:reverse(FacetsPlusFooter),
			       string:join(lists:reverse(ReverseFacets), "\n")
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

