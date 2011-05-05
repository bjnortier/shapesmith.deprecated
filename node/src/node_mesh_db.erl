-module(node_mesh_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).
-export([exists/1, mesh/1, stl/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).

exists(Hash) ->
    gen_server:call(?MODULE, {exists, Hash}, 30000).

mesh(Hash) -> 
    gen_server:call(?MODULE, {mesh, Hash}, 30000).

stl(Hash) ->
    gen_server:call(?MODULE, {stl, Hash}, 30000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

init([]) ->
    {ok, []}.

handle_call({exists, Hash}, _From, State) ->
    node_worker_access_logger:accessed(Hash),

    Reply = case lists:keyfind(Hash, 1, State) of
                {Hash, _} -> true;
                false -> false
            end,
    {reply, Reply, State};
handle_call({mesh, Hash}, _From, State) ->
    node_worker_access_logger:accessed(Hash),

    Reply = case lists:keyfind(Hash, 1, State) of
                {Hash, Mesh} -> 
                    {ok, Mesh};
                false -> 
                    node_log:info("meshing hash:~p~n", [Hash]),
                    Result = mochijson2:decode(
                               node_worker_server:call(
                                 mochijson2:encode(
                                   {struct, [{<<"tesselate">>, list_to_binary(Hash)}]}))),
                    case Result of
                        {struct, [{<<"error">>, E}]} ->
                            {error, E};
                        _ ->
                            {ok, Result}
                    end
            end,
    {reply, Reply, State};
handle_call({stl, Hash}, _From, State) ->
    node_worker_access_logger:accessed(Hash),

    {ok, DbDir} = application:get_env(node, db_dir),
    Filename = filename:join(
                 [filename:dirname(code:which(?MODULE)),
                  DbDir, Hash ++ ".stl"]),

    Msg = {struct, [{<<"type">>, <<"stl">>},
                    {<<"id">>, list_to_binary(Hash)},
                    {<<"filename">>, list_to_binary(Filename)}
                   ]},
    %% TODO: Error handling
    "\"ok\"" = node_worker_server:call(mochijson2:encode(Msg)),
    {ok, STL} = file:read_file(Filename),
    {reply, {ok, STL}, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 private                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

