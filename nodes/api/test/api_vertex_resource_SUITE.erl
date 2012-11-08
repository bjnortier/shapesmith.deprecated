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

-define(POST_URL, "http://localhost:8001/local/mydesign/vertex/").
-define(GET_URL_TEMPLATE, "http://localhost:8001/local/mydesign/vertex/~p").

create(_Config) ->
    VertexJSON = {[
        {<<"type">>, <<"sphere">>},
        {<<"parameters">>, {[
            {<<"r">>, 1.0}
        ]}}
    ]},

    {ok,{{"HTTP/1.1",201,_}, ReponseHeaders, ResponseBody}} = 
        httpc:request(post, {?POST_URL, [], "application/json", jiffy:encode(VertexJSON)}, [], []),

    api_ct_common:check_json_content_type(ReponseHeaders),
    {_, Location} = lists:keyfind("location", 1, ReponseHeaders),
    {match,[_,{Start,Len}]} = re:run(Location, "^http://localhost:8001/local/mydesign/vertex/([a-f0-9]+)$"),
    
    SHA = string:substr(Location, Start+1, Len),
    ok.
