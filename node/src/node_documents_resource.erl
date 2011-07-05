-module(node_documents_resource).
-export([
         init/1, 
         allowed_methods/2,
	 post_is_create/2,
         process_post/2,
	 resource_exists/2
        ]).


-include_lib("webmachine/include/webmachine.hrl").

-record(context, {}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['POST'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    case wrq:method(ReqData) of
        'POST' ->
            {true, ReqData, Context};
        'GET' ->
            {true, ReqData, Context}
    end.

post_is_create(ReqData, Context) ->
    {false, ReqData, Context}.

process_post(ReqData, Context) ->
    {ok, DB} = application:get_env(node, db_module),
    Id = DB:create(doc, []),
    ReqData1 = wrq:set_resp_body("{\"path\": \"/doc/" ++ Id ++ "\"}", ReqData),
    {true, ReqData1, Context}.


