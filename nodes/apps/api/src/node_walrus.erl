-module(node_walrus).
-export([render_template/2]).

render_template(Name, Ctx) when is_atom(Name) ->
    Filename = filename:join(
		 filename:dirname(
		  code:which(?MODULE)), atom_to_list(Name) ++ ".walrus"),
    {ok, Template} = file:read_file(Filename),
    walrus:render(Template, Ctx).
