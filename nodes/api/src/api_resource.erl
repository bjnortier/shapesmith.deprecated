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

-module(api_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([json_response/2, 
         create_session/2,
         forbidden_if_not_authorized/2]).
-include_lib("webmachine/include/webmachine.hrl").

json_response(JSON, ReqData) ->
    wrq:set_resp_header("Content-type", "application/json", 
                        wrq:set_resp_body(jiffy:encode(JSON), ReqData)).

create_session(ReqData, Username) ->
    {ok, AuthModule} = application:get_env(api, auth_module),
    {ok, CookieDomain} = application:get_env(api, cookie_domain),
    SessionSHA = AuthModule:create_session(Username),
    wrq:set_resp_header("Set-Cookie", "session=" ++ SessionSHA ++ ";Domain=" ++ CookieDomain ++ "; Path=/;", ReqData).

           
forbidden_if_not_authorized(ReqData, Context) ->
    is_authorized_common(ReqData, Context, 403, api_resource:json_response({[{<<"error">>, <<"Please sign in.">>}]}, ReqData)).

is_authorized_common(ReqData, Context, Code, NotAuthorisedReqData) ->
    {ok, AuthModule} = application:get_env(api, auth_module),
    case AuthModule:session_username(ReqData) of
        undefined ->
            {{halt, Code}, NotAuthorisedReqData, Context};
        Username ->
            User = wrq:path_info(user, ReqData),
            if 
                User =:= Username ->
                    {true, ReqData, Context};
                true ->     
                    {{halt, Code}, NotAuthorisedReqData, Context}
            end
    end.
