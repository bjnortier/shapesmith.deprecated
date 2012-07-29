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

-module(api_session_auth).
-export([session_username/1, 
         create_session/1, 
         delete_session/1, 
         add_session_walrus_ctx/2]).

    
session_username(ReqData) ->
    case wrq:get_cookie_value("session", ReqData) of
        undefined ->
            undefined;
        SessionSHA ->
            {ok, DB} = application:get_env(api, db_module),
            case DB:get(<<"_sessions">>, list_to_binary(SessionSHA)) of
                undefined ->
                    undefined;
                JSON ->
                    {Props} = jiffy:decode(JSON),
                    {_, UsernameBin} = lists:keyfind(<<"username">>, 1, Props),
                    binary_to_list(UsernameBin)
            end
    end.

create_session(Username) ->
    RFC1123Date = httpd_util:rfc1123_date(calendar:now_to_universal_time(erlang:now())),
    JSON = {[{<<"created">>, list_to_binary(RFC1123Date)},
             {<<"username">>, list_to_binary(Username)}]},
    SessionSHA = api_hash:hash_json(JSON),
    {ok, DB} = application:get_env(api, db_module),
    ok = DB:put(<<"_sessions">>, list_to_binary(SessionSHA), jiffy:encode(JSON)),
    SessionSHA.

delete_session(SessionSHA) ->
    {ok, DB} = application:get_env(api, db_module),
    DB:delete(<<"_sessions">>, list_to_binary(SessionSHA)),
    ok.

add_session_walrus_ctx(User, WalrusContext) ->
    [{session, [[{username, User}]]}|WalrusContext].
                      
    

    
    

    
    
