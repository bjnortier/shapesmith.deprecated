-module(node_db_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
         in_mem,
	 riak
	].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(in_mem, Config) ->
    node_mem_db:start_link(),
    Config;
init_per_testcase(riak, Config) ->
    ok = application:load(node),
    application:set_env(node, port, 8001),
    application:start(inets),
    ok = node:start(),
    Config.

end_per_testcase(in_mem, _Config) ->
    node_mem_db:stop();
end_per_testcase(riak, _Config) ->
    application:stop(node),
    application:unload(node).

in_mem(_Config) ->
    test(node_mem_db).

riak(_Config) ->
    test(node_riak_db).

test(DB) ->
    Bucket = bucket,
    Id = DB:create(Bucket, []),
    [] = DB:get(Bucket, Id),
    ok = DB:put(Bucket, "1", {1,2,3}),
    {1,2,3} = DB:get(Bucket, "1"),
    DB:put(Bucket, "1", <<4,5,6>>),
    <<4,5,6>> = DB:get(Bucket, "1"),
    ok.
