%%-*- mode: erlang -*-
-module(node_document_resource_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
         save_simple,
         save_boolean
	].

init_per_suite(Config) ->
    ok = application:load(node),
    application:set_env(node, port, 8001),
    application:start(inets),
    ok = application:set_env(node, db_module, node_riak_db),
    ok = node:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(node),
    application:unload(node),
    ok.

save_simple(_Config) ->
    %% Create
    {ok,{{"HTTP/1.1",200,_}, _, PostResponse}} = 
	httpc:request(post, {"http://localhost:8001/doc/", [], "application/json", "[]"}, [], []),
    {struct, [{<<"path">>, PathBin}]} = mochijson2:decode(PostResponse),

    %% Get
    DocPath = binary_to_list(PathBin),
    "/doc/" ++ DocId = DocPath,
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse}} = 
	httpc:request(get, {"http://localhost:8001" ++ DocPath, []}, [], []),
    [] = mochijson2:decode(GetResponse),

    %% Create a geometry node
    GeomProps = [{<<"type">>, <<"sphere">>},
                 {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}],
    {ok, GeomId} = node_geom_db:create({struct, GeomProps}),

    %% Simulate saving the geometry node, which includes the path
    SaveDoc = [
               list_to_binary("/geom/" ++ GeomId)
              ],
    {ok,{{"HTTP/1.1",204,_}, _, SaveResponse}} = 
	httpc:request(put, {"http://localhost:8001" ++ DocPath, [], "application/json", iolist_to_binary(mochijson2:encode(SaveDoc))}, [], []),
    [] = SaveResponse,

    %% Load the document
    {ok,{{"HTTP/1.1",200,_}, _, LoadResponse}} = 
	httpc:request(get, {"http://localhost:8001" ++ DocPath, []}, [], []),
    SaveDoc = mochijson2:decode(LoadResponse),

    {ok, {struct, _}} = node_master:mesh_geom(GeomId),

    ok.

save_boolean(_Config) ->
    %% Create
    {ok,{{"HTTP/1.1",200,_}, _, PostResponse}} = 
	httpc:request(post, {"http://localhost:8001/doc/", [], "application/json", "[]"}, [], []),
    {struct, [{<<"path">>, PathBin}]} = mochijson2:decode(PostResponse),
    DocPath = binary_to_list(PathBin),
    "/doc/" ++ DocId = DocPath,
    
    %% Create a geometry node
    GeomA = {struct, [{<<"type">>, <<"sphere">>},
                      {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    {ok, GeomIdA} = node_geom_db:create(GeomA),
    GeomB = {struct, [{<<"type">>, <<"cuboid">>},
                      {<<"parameters">>, {struct, [{<<"width">>, 1.0},
                                                   {<<"depth">>, 1.0},
                                                   {<<"height">>, 1.0}]}}]},
    {ok, GeomIdB} = node_geom_db:create(GeomB),
    GeomC = {struct, [{<<"type">>, <<"union">>},
                      {<<"children">>, [list_to_binary(GeomIdA),
                                        list_to_binary(GeomIdB)]}]},
    {ok, GeomIdC} = node_geom_db:create(GeomC),

    %% Simulate saving the geometry node, which includes the path
    SaveDoc = [
               list_to_binary("/geom/" ++ GeomIdC)
              ],
    {ok,{{"HTTP/1.1",204,_}, _, SaveResponse}} = 
	httpc:request(put, {"http://localhost:8001" ++ DocPath, [], "application/json", iolist_to_binary(mochijson2:encode(SaveDoc))}, [], []),
    [] = SaveResponse,

    %% Load the document
    {ok,{{"HTTP/1.1",200,_}, _, LoadResponse}} = 
	httpc:request(get, {"http://localhost:8001" ++ DocPath, []}, [], []),
    SaveDoc = mochijson2:decode(LoadResponse),

    {ok, {struct, _}} = node_master:mesh_geom(GeomIdC),

    ok.
    
    
