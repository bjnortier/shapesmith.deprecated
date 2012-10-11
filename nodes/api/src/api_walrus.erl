-module(api_walrus).
-export([render_template/2]).

render_template(Name, Ctx) when is_atom(Name) ->
    {ok, App} = application:get_application(),
    Filename = filename:join(
                 [code:priv_dir(App),
                  atom_to_list(Name) ++ ".walrus"]),
    {ok, Template} = file:read_file(Filename),
    walrus:render(Template, Ctx).
