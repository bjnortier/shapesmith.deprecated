-module(node_purge_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
	 purge
	].

init_per_testcase(_TestCase, Config) ->
    application:start(inets),
    ok = application:load(node),
    ok = application:set_env(node, port, 8001),
    ok = application:set_env(node, db_dir, filename:join(["../test_db/", ?MODULE])),
    ok = application:set_env(node, brep_purge_job_period, 1000), %% After one second
    ok = application:set_env(node, brep_purge_expiry, 2), % Expires after 2 secs
    ok = node:start(),

    %% Clean the directory
    TestDb = filename:join(
               [filename:dirname(code:which(?MODULE)), "../test_db/", ?MODULE]),
    ok = filelib:ensure_dir(TestDb),
    file:make_dir(TestDb),
    {ok, Files} = file:list_dir(TestDb),
    [file:delete(filename:join(TestDb, File)) || File <- Files],
    {ok, []} = file:list_dir(TestDb),
    
    Config.

end_per_testcase(_TestCase, _Config) ->
    application:stop(node),
    ok = application:unload(node),
    ok.

purge(_Config) ->
    Geometry = {struct, [{<<"type">>, <<"sphere">>},
                         {<<"parameters">>, {struct, [{<<"radius">>, 1}]}}]},
    {ok, Id} = node_master:create_geom(Geometry),
    {ok, {struct, _}} = node_master:mesh_geom(Id),

    %% Wait for purge
    timer:sleep(5000),
    BRepLog1 = node_brep_db:log(),
    1 = length(BRepLog1),
    purged = proplists:get_value(node_geom_db:hash(Id), BRepLog1),

    %% Mesh the geometry (requires brep)
    {ok, {struct, _}} = node_master:mesh_geom(Id),

    %% It must have been read from file
    BRepLog2 = node_brep_db:log(),
    1 = length(BRepLog2),
    from_serialized = proplists:get_value(node_geom_db:hash(Id), BRepLog2),

    ok.
