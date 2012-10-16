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

-module(ui_modeller_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([
        init/1, 
        allowed_methods/2,
        is_authorized/2,
        content_types_provided/2,
        resource_exists/2,
        provide_content/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
        {ok, {}}.

allowed_methods(ReqData, Context) -> 
        {['GET'], ReqData, Context}.

is_authorized(ReqData, Context) ->
        ui_resource:redirect_to_signin_if_not_authorized(ReqData, Context).

content_types_provided(ReqData, Context) ->
        {[{"text/html", provide_content}], ReqData, Context}.

resource_exists(ReqData, Context) ->
        {true, ReqData, Context}.

provide_content(ReqData, Context) ->
        User = wrq:path_info(user, ReqData),
        Design = wrq:path_info(design, ReqData),
        Commit = wrq:get_qs_value("commit", ReqData),
        {ok, Host} = application:get_env(api, host),
        {ok, ScriptsEnv} = application:get_env(ui, js_scripts_environment),
        Logo = case application:get_env(ui, logo) of
                {ok, Path} -> Path;
                undefined -> undefined
        end,
        WalrusContext = [
                        {username, User},
                        {design, Design},
                        {commit, Commit},
                        {host, Host},
                        {logo, Logo}
                        ],
        
        {ok, AuthModule} = application:get_env(api, auth_module),
        WalrusContext1 = AuthModule:add_session_walrus_ctx(User, WalrusContext),
        
        Rendered = ui_walrus:render_template(ui_views_modeller, WalrusContext1),
        {Rendered, ui_resource:prevent_caching(ReqData), Context}.


