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

-module(node_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([json_response/2, prevent_caching/1]).

json_response(JSON, ReqData) ->
    wrq:set_resp_header("Content-type", "application/json", 
			wrq:set_resp_body(jiffy:encode(JSON), ReqData)).

prevent_caching(ReqData) ->
    wrq:set_resp_header(
      "Cache-Control", "no-cache, must-revalidate", ReqData).
