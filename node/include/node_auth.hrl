is_authorized(ReqData, Context) ->
    {ok, AuthModule} = application:get_env(node, auth_module),
    case AuthModule:session_username(ReqData) of
        undefined ->
	    Location = node_resource:base_url(ReqData) ++ "/signin",
	    {{halt, 302}, wrq:set_resp_header("Location", Location, ReqData), Context};
        Username ->
	    User = wrq:path_info(user, ReqData),
	    if 
		User =:= Username ->
		    {true, ReqData, Context};
		true ->	    
		    Location = node_resource:base_url(ReqData) ++ "/signin",
		    {{halt, 302}, wrq:set_resp_header("Location", Location, ReqData), Context}
	    end
    end.

