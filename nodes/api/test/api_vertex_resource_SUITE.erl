-module(api_vertex_resource_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
    [
        create
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

-define(URL, "http://localhost:8001/local/mydesign/vertex/").

create(_Config) ->
    VertexJSON = {[
        {<<"type">>, <<"sphere">>},
        {<<"parameters">>, {[
            {<<"r">>, 1.0}
        ]}}
    ]},

    {ok,{{"HTTP/1.1",404,_}, _, _}} = 
        httpc:request(get, {?URL ++ "ab123", []}, [], []),

    {ok,{{"HTTP/1.1",201,_}, ReponseHeaders0, _}} = 
        httpc:request(post, {?URL, [], "application/json", jiffy:encode(VertexJSON)}, [], []),

    api_ct_common:check_json_content_type(ReponseHeaders0),
    {_, Location} = lists:keyfind("location", 1, ReponseHeaders0),
    {match,[_,{Start,Len}]} = re:run(Location, "^http://localhost:8001/local/mydesign/vertex/([a-f0-9]+)$"),
    
    SHA = string:substr(Location, Start+1, Len),

    {ok,{{"HTTP/1.1",200,_}, ReponseHeaders1, ResponseBody}} = 
        httpc:request(get, {?URL ++ SHA, []}, [], []),

    VertexJSON = jiffy:decode(ResponseBody),
    ok.
