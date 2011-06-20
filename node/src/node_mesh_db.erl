-module(node_mesh_db).
-export([mesh/2, stl/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

mesh(WorkerPid, Hash) ->
    node_log:info("meshing hash:~p~n", [Hash]),
    case node_worker_pool:call(
	   WorkerPid,
	   mochijson2:encode(
	     {struct, [{<<"tesselate">>, list_to_binary(Hash)}]})) of

	{error, Reason} ->
	    {error, Reason};
	JSON ->
	    case mochijson2:decode(JSON) of
		{struct, [{<<"error">>, E}]} ->
		    {error, E};
		Result ->
		    {ok, Result}
	    end
    end.

stl(WorkerPid, Hash) ->
    {ok, DbDir} = application:get_env(node, db_dir),
    Filename = filename:join(
                 [filename:dirname(code:which(?MODULE)),
                  DbDir, Hash ++ ".stl"]),

    Msg = {struct, [{<<"type">>, <<"stl">>},
                    {<<"id">>, list_to_binary(Hash)},
                    {<<"filename">>, list_to_binary(Filename)}
                   ]},
    case node_worker_pool:call(WorkerPid, mochijson2:encode(Msg)) of
	"\"ok\"" ->
	    file:read_file(Filename);
	{error, Reason} ->
	    {error, Reason}
    end.
