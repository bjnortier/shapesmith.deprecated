-module(node_mesh_resource).
-export([
         init/1, 
         allowed_methods/2,
         resource_exists/2,
	 content_types_provided/2,
	 provide_content/2
        ]).


-include_lib("webmachine/include/webmachine.hrl").

-record(context, {id}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['GET'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    Id = wrq:path_info(id, ReqData),
    Exists = node_master:exists(Id),
    {Exists, ReqData, Context#context{id = Id}}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    Id = Context#context.id,
    case node_master:mesh_geom(Id) of
	{ok, Mesh} ->
	    {mochijson2:encode(Mesh), ReqData, Context};
	{error, _Err} ->
	    ErrorJSON = mochijson2:encode({struct, [{<<"error">>, <<"Could not mesh geometry">>}]}),
	    {{halt, 500}, wrq:set_resp_body(ErrorJSON, ReqData), Context}
    end.

