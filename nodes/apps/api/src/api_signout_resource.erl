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

-module(api_signout_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([
	 init/1, 
         allowed_methods/2,
	 content_types_provided/2,
	 resource_exists/2,
	 provide_content/2
        ]).
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
    {ok, []}.

allowed_methods(ReqData, Context) -> 
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", provide_content}], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {true, ReqData, Context}.

provide_content(ReqData, Context) ->
    case wrq:get_cookie_value("session", ReqData) of
	undefined ->
	    ok;
	SessionSHA ->
	    {ok, AuthModule} = application:get_env(api, auth_module),
	    AuthModule:delete_session(SessionSHA)
    end,
    {ok, Host} = application:get_env(api, host),
    {{halt, 302}, wrq:set_resp_header("Location", Host, ReqData), Context}.
