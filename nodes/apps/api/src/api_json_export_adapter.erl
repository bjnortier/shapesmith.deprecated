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

-module(api_json_export_adapter).
-author('Benjamin Nortier <bjnortier@gmail.com>').

-export([methods/1, get/3, update_reqdata_for_get/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 public                                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

methods(_ReqData) ->
    ['GET'].

get(ReqData, User, Design) ->
    SHA = wrq:path_info(sha, ReqData),
    case api_db:get(User, Design, commit, SHA) of
        undefined ->
            undefined;
        {Props} ->
            {_, GeomSHAs} = lists:keyfind(<<"geoms">>, 1, Props),
            lists:map(fun(GeomSHABin) ->
                              recursive_geom_get(User, Design, binary_to_list(GeomSHABin))
                      end,
                      GeomSHAs)
    end.

update_reqdata_for_get(ReqData, _User, Design) ->
    ReqData1 =  wrq:set_resp_header("Content-disposition",
				    "attachment; filename=" ++ Design ++ ".shapesmith",
				    ReqData),
    ReqData2 =  wrq:set_resp_header("Content-Type",
				    "application/json",
				    ReqData1),
    ReqData2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 private                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

recursive_geom_get(User, Design, SHA) ->
    Geom = api_db:get(User, Design, geom, SHA),
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
    {NewProps}.
