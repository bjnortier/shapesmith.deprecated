-module(api_mesh_resource_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
         simple
	].

init_per_suite(Config) ->
    ok = api_deps:start_with_api(),
    Config.

end_per_suite(_Config) ->
    api_deps:stop_with_api().

init_per_testcase(_Testcase, Config) ->
    {ok, _} = api_mem_db:start_link(),
    Config.

end_per_testcase(_Testcase, _Config) ->
    api_mem_db:stop(),
    ok.

simple(_Config) ->
    %% Not found
    {ok,{{"HTTP/1.1",404,_}, _, _}} = 
     	httpc:request(get, {"http://localhost:8001/local/iphonedock/mesh/abc", []}, [], []),

    %% Create
    GeomA = {[{<<"type">>, <<"sphere">>},
	      {<<"origin">>, {[{<<"x">>, 0},
			       {<<"y">>, 0},
			       {<<"z">>, 0}]}},
	      {<<"parameters">>, {[{<<"r">>, 1.0}]}}]},
    {ok, Id} = api_master:create_geom("local", "iphonedock", GeomA),
    
    %% Get
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse}} = 
	httpc:request(get, {"http://localhost:8001/local/iphonedock/mesh/" ++ Id, []}, [], []),
    {_} = jiffy:decode(iolist_to_binary(GetResponse)),

    ok.
    
