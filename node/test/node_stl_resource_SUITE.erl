-module(node_stl_resource_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
	 export
	].

init_per_suite(Config) ->
    ok = application:load(node),
    application:set_env(node, port, 8001),
    application:set_env(node, db_module, node_mem_db),
    ok = node:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(node),
    application:unload(node),
    ok.

init_per_testcase(_Testcase, Config) ->
    {ok, _} = node_mem_db:start_link(),
    Config.

end_per_testcase(_Testcase, _Config) ->
    node_mem_db:stop(),
    ok.


export(_Config) ->
    %% Not found
    {ok,{{"HTTP/1.1",404,_}, _, _NotFoundResponse}} = 
      	httpc:request(get, {"http://localhost:8001/local/iphonedock/stl/abc", []}, [], []),

    GeomCreateURL = "http://localhost:8001/local/iphonedock/geom/", 
    GeomA = {[{<<"type">>, <<"cuboid">>},
              {<<"origin">>, {[{<<"x">>, 0}, {<<"y">>, 0}, {<<"z">>, 0}]}},
              {<<"parameters">>, {[{<<"u">>, 1}, {<<"v">>, 1}, {<<"w">>, 1}]}}]},
    
    {ok,{{"HTTP/1.1",200,_}, _, ResponseA}} = 
	httpc:request(post, {GeomCreateURL, [], "application/json", jiffy:encode(GeomA)}, [], []),
    {[{<<"path">>, _},
      {<<"SHA">>, GeomSHABin}]} = jiffy:decode(iolist_to_binary(ResponseA)),

    %% Create commit
    Commit = {[{<<"parent">>, <<"not_used">>},
               {<<"geoms">>, [GeomSHABin]}]},
    CommitCreateURL = "http://localhost:8001/local/iphonedock/commit/", 
    {ok,{{"HTTP/1.1",200,_}, CreateHeaders, PostResponse}} = 
	httpc:request(post, {CommitCreateURL, [], "application/json", jiffy:encode(Commit)}, [], []),
    {[{<<"path">>, _},
      {<<"SHA">>, CommitSHABin}]} = jiffy:decode(iolist_to_binary(PostResponse)),
    CommitSHA = binary_to_list(CommitSHABin),
    
    %% Get
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse}} = 
     	httpc:request(get, {"http://localhost:8001/local/iphonedock/stl/" ++ CommitSHA, []}, [], []),
    "solid " ++ _ = GetResponse,

    ok.
    
