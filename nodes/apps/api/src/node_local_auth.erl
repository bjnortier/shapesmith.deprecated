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

-module(node_local_auth).
-export([session_username/1, 
         create_session/1, 
         delete_session/1, 
         add_session_walrus_ctx/2]).

session_username(_ReqData) ->
    "local".

create_session(_Username) ->
    throw(local_auth_should_not_create_a_session).

delete_session(_SessionSHA) ->
    ok.

add_session_walrus_ctx(_User, WalrusContext) ->
    WalrusContext.
