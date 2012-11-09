-module(api_replace_vertex_resource_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
    [
        replace
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

-define(DESIGN_URL, "http://localhost:8001/local/mydesign").

replace(_Config) ->
    CreateVertex = fun(VertexJSON) ->
        {ok,{{"HTTP/1.1",201,_}, _, ResponseBody}} = 
            httpc:request(post, {?DESIGN_URL ++ "/vertex", [], "application/json", jiffy:encode(VertexJSON)}, [], []),
        jiffy:decode(ResponseBody)
    end,

    SHAA = CreateVertex({[{<<"name">>, <<"a">>}]}),
    SHAB = CreateVertex({[{<<"name">>, <<"b">>}]}),
    SHAC = CreateVertex({[{<<"name">>, <<"c">>}]}),
    SHAD = CreateVertex({[{<<"name">>, <<"d">>}]}),

    GraphJSON0 = {[
        {<<"edges">>, [
            {[{SHAA, [SHAB, SHAC]}]},
            {[{SHAB, []}]},
            {[{SHAC, []}]}
        ]}
    ]},

    {ok,{{"HTTP/1.1",201,_}, _, ResponseBody0}} = 
        httpc:request(post, {?DESIGN_URL ++ "/graph", [], "application/json", jiffy:encode(GraphJSON0)}, [], []),
    GraphSHA = binary_to_list(jiffy:decode(ResponseBody0)),

    ReplaceJSON = {[{<<"original">>, SHAB}, {<<"replacement">>, SHAD}]},
    {ok,{{"HTTP/1.1",201,_}, _, ResponseBody1}} = 
        httpc:request(post, {?DESIGN_URL  ++ "/graph/" ++ GraphSHA ++ "/replace", [], "application/json", jiffy:encode(ReplaceJSON)}, [], []),

    {[{<<"new_graph_sha">>, _},
      {<<"notify_parents">>, NotifyParents}]} = jiffy:decode(ResponseBody1),

    [SHAA] = NotifyParents,
    ok.

