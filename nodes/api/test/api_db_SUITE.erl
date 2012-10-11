%% -*- mode: erlang -*-
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% Copyright 2011 Benjamin Nortier
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.

-module(api_db_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
         in_mem,
	 %% Not for normal users riak,
	 disk
	].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    application:stop(api),
    application:unload(api),
    ok.

init_per_testcase(in_mem, Config) ->
    api_mem_db:start_link(),
    Config;
init_per_testcase(riak, Config) ->
    ok = application:load(api),
    application:set_env(api, port, 8001),
    application:set_env(api, riak_host, {"127.0.0.1", 8087}),
    application:start(inets),
    ok = api:start(),
    Config;
init_per_testcase(disk, Config) ->
    application:set_env(api, disk_db_dir, "test_db"),
    Config.

end_per_testcase(in_mem, _Config) ->
    api_mem_db:stop();
end_per_testcase(riak, _Config) ->
    application:stop(api),
    application:unload(api);
end_per_testcase(disk, _Config) ->
    {ok, DbDir} = application:get_env(api, disk_db_dir),
    TestDBDir = filename:join(
		  [filename:dirname(code:which(?MODULE)),
		   "..", DbDir]),
    file:del_dir(TestDBDir),
    ok.

in_mem(_Config) ->
    test(api_mem_db).

riak(_Config) ->
    test(api_riak_db).

disk(_Config) ->
    test(api_disk_db).

test(DB) ->
    Bucket = <<"bucket">>,
    Id = <<"some/id">>,
    undefined = DB:get(Bucket, <<"abc/123">>),
    ok = DB:put(Bucket, Id, <<1,2,3>>),
    <<1,2,3>> = DB:get(Bucket, Id),
    DB:put(Bucket, Id, <<4,5,6>>),
    <<4,5,6>> = DB:get(Bucket, Id),
    ok = DB:delete(Bucket, Id),
    undefined = DB:get(Bucket, Id),
    ok = DB:put(Bucket, Id, <<1,2,3>>),
    ok = DB:delete(Bucket, Id),
    undefined = DB:get(Bucket, Id),
    ok.
