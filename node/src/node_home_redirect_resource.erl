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

-module(node_home_redirect_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([init/1, resource_exists/2, moved_temporarily/2, previously_existed/2]).
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

moved_temporarily(ReqData, Context) ->
    {{true, "/index.html"}, ReqData, Context}.

previously_existed(ReqData, Context) -> {true, ReqData, Context}.

resource_exists(ReqData, Context) -> {false, ReqData, Context}.
