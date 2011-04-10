-module(node_documents_resource).
-export([
         init/1, 
         allowed_methods/2,
	 content_types_provided/2,
	 provide_content/2,
	 post_is_create/2,
         process_post/2,
	 resource_exists/2
        ]).


-include_lib("webmachine/include/webmachine.hrl").

-record(context, {}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['GET', 'POST'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    case wrq:method(ReqData) of
        'POST' ->
            {true, ReqData, Context};
        'GET' ->
            {true, ReqData, Context}
    end.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    DocIds = node_document_db:ids(),
    JSON = lists:map(fun list_to_binary/1, DocIds),
    {mochijson2:encode(JSON), ReqData, Context}.


post_is_create(ReqData, Context) ->
    {false, ReqData, Context}.

process_post(ReqData, Context) ->
    Id = node_document_db:create(),
    ReqData1 = wrq:set_resp_body("{\"path\": \"/doc/" ++ Id ++ "\"}", ReqData),
    {true, ReqData1, Context}.


