-module(node_geom_resource).
-export([
         init/1, 
         allowed_methods/2,
	 content_types_provided/2,
	 provide_content/2,
         content_types_accepted/2,
         accept_content/2,
	 post_is_create/2,
	 allow_missing_post/2,
	 create_path/2,
	 resource_exists/2,
         malformed_request/2
        ]).


-include_lib("webmachine/include/webmachine.hrl").

%% The incoming JSON is transformed to convert paths (e.g. "/geom/1") to
%% ids for children
-record(context, {id, geom_json}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['GET', 'POST', 'PUT'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    case wrq:method(ReqData) of
        'POST' ->
            {false, ReqData, Context};
        _ ->
            Exists = node_geom_db:exists(Context#context.id),
            {Exists, ReqData, Context}
    end.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    Id = Context#context.id,
    case wrq:get_qs_value("recursive", "false" ,ReqData) of
        "false" ->
            Geometry = node_master:geometry(Id),
            {mochijson2:encode(transform_ids_to_paths(Geometry)), ReqData, Context};
        "true" ->
            Geometry = node_master:recursive_geometry(Id),
            {mochijson2:encode(transform_ids_to_paths(Geometry)), ReqData, Context}
    end.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_content}], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}. 

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    %% FIXME: This is not the correct path
    {"/geom/1", ReqData, Context}. 

accept_content(ReqData, Context) ->
    {true, ReqData, Context}.


malformed_request(ReqData, Context) ->
    Method = wrq:method(ReqData),
    malformed_request(ReqData, Context, Method).
    

malformed_request(ReqData, Context, 'GET') ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {true, wrq:set_resp_body("missing id: /geom/<id>", ReqData), Context};
        Id when is_list(Id) ->
            {false, ReqData, Context#context{id = Id}}
    end;
malformed_request(ReqData, Context, 'PUT') ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {true, wrq:set_resp_body("missing id: /geom/<id>", ReqData), Context};
        Id when is_list(Id) ->
	    case valid_json(wrq:req_body(ReqData)) of
		{true, JSON} ->
		    case node_master:update_geom(Id, transform_paths_to_ids(JSON)) of
			ok ->
			    {false, ReqData, Context#context{id = Id}};
			{error, Reason = {validation, _}} ->
			    ReqData1 = error_response(Reason, ReqData),
			    {true, ReqData1, Context};
			Error = {error, _Reason} ->
			    node_log:error("~p~n", [Error]),
			    ReqData1 = error_response(Error, ReqData),
			    {{halt, 500}, ReqData1, Context}
		    end;
		false ->
		    {true, ReqData, Context}
	    end
    end;
malformed_request(ReqData, Context, 'POST') ->
    case valid_json(wrq:req_body(ReqData)) of
	{true, JSON} ->
	    JSONWithIds = transform_paths_to_ids(JSON),
	    case node_master:create_geom(JSONWithIds) of
		{ok, Id} ->
		    Path = io_lib:format("/geom/~s", [Id]),
		    ReqData1 = wrq:set_resp_body(
				 mochijson2:encode({struct, [{<<"path">>, iolist_to_binary(Path)}]}), ReqData),
		    {false, ReqData1, Context};
		{error, Reason = {validation, _}} ->
		    ReqData1 = error_response(Reason, ReqData),
		    {true, ReqData1, Context};
		Error = {error, _Reason} ->
		    node_log:error("~p~n", [Error]),
		    ReqData1 = error_response(Error, ReqData),
		    {{halt, 500}, ReqData1, Context}

	    end;
	false ->
	    ReqData1 = wrq:set_resp_body(<<"\"invalid json\"">>, ReqData),
	    {true, ReqData1, Context}
    end.

error_response({validation, ErrorParams}, ReqData) ->
    wrq:set_resp_body(
      mochijson2:encode(
	{struct, [{<<"validation">>, ErrorParams}]}), 
      ReqData);
error_response({error,worker_timeout}, ReqData) ->
    wrq:set_resp_body(
      mochijson2:encode(
	{struct, [{<<"error">>, <<"timeout">>}]}), 
      ReqData);
error_response(_, ReqData) ->
    wrq:set_resp_body(
      mochijson2:encode(
	{struct, [{<<"error">>, <<"internal error">>}]}), 
      ReqData).



valid_json(Body) ->
    try 
	{true, mochijson2:decode(Body)}
    catch
	_:_ ->
            node_log:info("invalid JSON: ~p", [Body]),
	    false
    end.


transform_paths_to_ids(JSON = {struct, Props}) ->
    case lists:keyfind(<<"children">>, 1, Props) of
        false ->
            JSON;
        {<<"children">>, Paths} ->
            NewChildren = lists:map(fun(Path) ->
                                            "/geom/" ++ Id = binary_to_list(Path),
                                            list_to_binary(Id)
                                    end,
                                    Paths),
            {struct, lists:keyreplace(<<"children">>, 1, Props, {<<"children">>, NewChildren})}
    end.

transform_ids_to_paths({struct, Props}) ->
    io:format("!!! ~p~n", [Props]),
    {<<"id">>, IdBin} = lists:keyfind(<<"id">>, 1, Props),
    Props1 = lists:keyreplace(<<"id">>, 1, Props, 
                             {<<"path">>, list_to_binary("/geom/" ++ binary_to_list(IdBin))}),
    case lists:keyfind(<<"children">>, 1, Props1) of
        false ->
            {struct, Props1};
        {<<"children">>, IdsOrNestedStructs} ->
            NewChildren = lists:map(fun(Id) when is_binary(Id) ->
                                            list_to_binary("/geom/" ++ binary_to_list(Id));
                                       (Structs) when is_tuple(Structs) ->
                                            transform_ids_to_paths(Structs)
                                    end,
                                    IdsOrNestedStructs),
            {struct, lists:keyreplace(<<"children">>, 1, Props1, {<<"children">>, NewChildren})}
    end.




