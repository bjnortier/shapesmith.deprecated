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


%% Mocking with Meck created some problems with bcrypt (perhaps because of the C functions?), 
%% so this module is a proxy for the bcrypt library. 

-module(api_bcrypt).
-export([hashpw/2, gen_salt/0]).

hashpw(Password, SaltOrHash) ->
    bcrypt:hashpw(Password, SaltOrHash).

gen_salt() ->
    bcrypt:gen_salt().
