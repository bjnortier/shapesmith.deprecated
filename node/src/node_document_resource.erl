-module(node_document_resource).
-export([
         init/1, 
         allowed_methods/2,
	 content_types_provided/2,
	 provide_content/2,
         content_types_accepted/2,
         accept_content/2,
	 resource_exists/2,
         malformed_request/2
        ]).


-include_lib("webmachine/include/webmachine.hrl").

-record(context, {id, json_obj}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['GET', 'PUT'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {ok, DB} = application:get_env(node, db_module),
    Exists = DB:exists(doc, Context#context.id),
    {Exists, ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    Id = Context#context.id,
    {ok, DB} = application:get_env(node, db_module),
    case DB:get(doc, Id) of
        undefined ->
            {"{\"error\" : \"not found\"", ReqData, Context};
        GeomIds ->
            F = fun(GeomId) ->
                        list_to_binary("/geom/" ++ GeomId)
                end,
            {mochijson2:encode(lists:map(F, GeomIds)), ReqData, Context}
    end.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_content}], ReqData, Context}.

accept_content(ReqData, Context) ->
    Id = Context#context.id,
    GeomIds = lists:map(fun(Path) ->
                                "/geom/" ++ GeomId = binary_to_list(Path),
                                GeomId
                        end,
                        Context#context.json_obj),
    {ok, DB} = application:get_env(node, db_module),
    case DB:put(doc, Context#context.id, GeomIds) of
        ok ->
            Path = io_lib:format("/doc/~s", [Id]),
            io:format("updated doc: ~s~n", [Path]),
            {true, ReqData, Context};
        {error, Reason} ->
            io:format("ERR: ~p~n", [Reason]),
            {false, Reason, Context}
    end.


malformed_request(ReqData, Context) ->
    Method = wrq:method(ReqData),
    malformed_request(ReqData, Context, Method).
    

malformed_request(ReqData, Context, 'GET') ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {true, wrq:set_resp_body("missing id: /doc/<id>", ReqData), Context};
        Id when is_list(Id) ->
            {false, ReqData, Context#context{id = Id}}
    end;
malformed_request(ReqData, Context, 'PUT') ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {true, wrq:set_resp_body("missing id: /doc/<id>", ReqData), Context};
        Id when is_list(Id) ->
            malformed_json_request(ReqData, Context#context{id = Id})
    end.

malformed_json_request(ReqData, Context) ->
    Body = wrq:req_body(ReqData),
    try 
	JSONObj = mochijson2:decode(Body),
        {false, ReqData, Context#context{ json_obj = JSONObj }}
    catch
	A:B ->
            node_log:info("malformed request: ~p -> ~p:~p", [Body, A, B]),
	    {true, wrq:set_resp_body("invalid JSON", ReqData), Context}
    end.

