-module(node_stl_resource).
-export([
         init/1, 
         allowed_methods/2,
	 content_types_provided/2,
	 provide_content/2,
	 resource_exists/2,
         malformed_request/2
        ]).


-include_lib("webmachine/include/webmachine.hrl").

-record(context, {id, json_obj}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['GET'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    Exists = node_master:exists(Context#context.id),
    {Exists, ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/sla", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    Id = Context#context.id,
    {ok, STL} = node_master:stl(Id),
    ReqData1 =  wrq:set_resp_header("Content-disposition",
                                    "attachment; filename=" ++ Id ++ ".stl",
                                    ReqData),
    ReqData2 =  wrq:set_resp_header("Content-Type",
                                    "application/sla",
                                    ReqData1),
    {STL, ReqData2, Context}.

malformed_request(ReqData, Context) ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {true, wrq:set_resp_body("missing id: /stl/<id>", ReqData), Context};
        Id when is_list(Id) ->
            {false, ReqData, Context#context{id = Id}}
    end.
