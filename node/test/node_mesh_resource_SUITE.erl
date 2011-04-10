-module(node_mesh_resource_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
	 simple
	].

init_per_suite(Config) ->
    ok = application:load(node),
    application:set_env(node, port, 8001),
    application:start(inets),
    ok = node:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(node),
    application:unload(node),
    ok.

simple(_Config) ->
    %% Not found
    {ok,{{"HTTP/1.1",404,_}, _, _}} = 
	httpc:request(get, {"http://localhost:8001/mesh/abc", []}, [], []),

    %% Create
    GeomA = {struct, [{<<"type">>, <<"sphere">>},
                      {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    {ok, Id} = node_master:create_geom(GeomA),
    
    %% Get
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse}} = 
	httpc:request(get, {"http://localhost:8001/mesh/" ++ Id, []}, [], []),
    {struct, _} = mochijson2:decode(GetResponse),

    ok.
    
