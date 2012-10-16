%% -*- mode: erlang -*-
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% Copyright 2011-2012 Benjamin Nortier
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

-module(ui_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([
    prevent_caching/1, 
    redirect_to_signin_if_not_authorized/2,
    redirect_to_designs_if_username_known/2
    ]).
-include_lib("webmachine/include/webmachine.hrl").

prevent_caching(ReqData) ->
    wrq:set_resp_header(
      "Cache-Control", "no-cache, must-revalidate", ReqData).

redirect_to_designs_if_username_known(ReqData, Context) ->
    {ok, AuthModule} = application:get_env(api, auth_module),
    case AuthModule:session_username(ReqData) of
        undefined ->
            {true, ReqData, Context};
        Username ->
            {ok, Host} = application:get_env(api, host),
            Location = Host ++ "/" ++ Username ++ "/designs/",
            {{halt, 302}, wrq:set_resp_header("Location", Location, ReqData), Context}
    end.

redirect_to_signin_if_not_authorized(ReqData, Context) ->
    {ok, Host} = application:get_env(api, host),
    Location = Host ++ "/ui/signin",
    {ok, AuthModule} = application:get_env(api, auth_module),
    case AuthModule:session_username(ReqData) of
        %% No session
        undefined ->
            {{halt, 302},  wrq:set_resp_header("Location", Location, ReqData), Context};
        Username ->
            User = wrq:path_info(user, ReqData),
            if 
                User =:= Username ->
                    {true, ReqData, Context};
                %% Session user doesn't match the user in the path
                true ->     
                    {{halt, 302},  wrq:set_resp_header("Location", Location, ReqData), Context}
            end
    end.
