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

-module(ui_walrus).
-export([render_template/2]).

render_template(Name, Ctx) when is_atom(Name) ->
    {ok, App} = application:get_application(?MODULE),
    Filename = filename:join(
                 [code:priv_dir(App),
                  atom_to_list(Name) ++ ".walrus"]),
    {ok, Template} = file:read_file(Filename),
    walrus:render(Template, Ctx).
