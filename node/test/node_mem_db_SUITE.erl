-module(node_mem_db_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
         crud
	].

init_per_testcase(TestCase, Config) ->
    node_mem_db:start_link(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    node_mem_db:stop(),
    ok.
    
crud(_Config) ->
    Bucket = bucket,
    Id = node_mem_db:create(Bucket, []),
    [] = node_mem_db:get(Bucket, Id),
    node_mem_db:put(Bucket, 1, {1,2,3}),
    {1,2,3} = node_mem_db:get(Bucket, 1),
    node_mem_db:put(Bucket, 1, <<4,5,6>>),
    <<4,5,6>> = node_mem_db:get(Bucket, 1),
    ok.
