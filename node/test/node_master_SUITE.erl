-module(node_master_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
         create_simple,
         create_boolean,
         serialize_geom,
         serialize_geom_and_brep,
         serialize_boolean,
         serialize_deep_boolean
	].

init_per_suite(Config) ->
    ok = application:start(inets),
    Config.

end_per_suite(_Config) ->
    application:stop(node),
    ok.
    

init_per_testcase(_TestCase, Config) ->
    ok = application:load(node),
    ok = application:set_env(node, port, 8001),
    ok = application:set_env(node, db_dir, filename:join(["../test_db/", ?MODULE])),
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

create_simple(_Config) ->
    Geometry = {struct, [{<<"type">>, <<"sphere">>},
                         {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    {ok, Id} = node_master:create_geom(Geometry),
    {ok, {struct, _}} = node_master:mesh_geom(Id),
    {ok, Stl} = node_master:stl(Id),
    true = is_binary(Stl),
    ok.

create_boolean(_Config) ->
    Geometry1 = {struct, [{<<"type">>, <<"sphere">>},
                          {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    Geometry2 = {struct, [{<<"type">>, <<"sphere">>},
                          {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}},
                          {<<"transforms">>, [
                                              {struct, [{<<"type">>, <<"translate">>},
                                                        {<<"parameters">>, {struct, [{<<"dx">>, 0.5},
                                                                                     {<<"dy">>, 0.5},
                                                                                     {<<"dz">>, 0.5}]}}]}
                                             ]}]},

    {ok, Id1} = node_master:create_geom(Geometry1),
    {ok, Id2} = node_master:create_geom(Geometry2),

    Geometry3 = {struct, [{<<"type">>, <<"union">>},
                          {<<"children">>, [
                                            list_to_binary(Id1),
                                            list_to_binary(Id2)
                                           ]}]},
    {ok, Id3} = node_master:create_geom(Geometry3),

    {ok, {struct, _}} = node_master:mesh_geom(Id3),
    {ok, Stl} = node_master:stl(Id3),
    true = is_binary(Stl),

    ok.

serialize_geom(_Config) ->
    Geometry = {struct, [{<<"type">>, <<"sphere">>},
                         {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    {ok, Id} = node_master:create_geom(Geometry),
    ok = node_master:serialize_geom(Id),

    %% Restart application
    application:stop(node),
    application:start(node),
    
    %% Deserialize and load mesh
    ok = node_master:deserialize_geom(Id),
    {ok, {struct, _}} = node_master:mesh_geom(Id),

    ok.

serialize_geom_and_brep(_Config) ->
    Geometry = {struct, [{<<"type">>, <<"sphere">>},
                         {<<"parameters">>, {struct, [{<<"radius">>, 1}]}}]},
    {ok, Id} = node_master:create_geom(Geometry),
    {ok, {struct, _}} = node_master:mesh_geom(Id),
    ok = node_master:serialize_geom(Id),
    ok = node_master:serialize_brep(Id),

    %% Restart application
    application:stop(node),
    application:start(node),
    
    %% Deserialize and load mesh
    ok = node_master:deserialize_geom(Id),
    {ok, {struct, _}} = node_master:mesh_geom(Id),

    ok.

serialize_boolean(_Config) ->
    Geometry1 = {struct, [{<<"type">>, <<"sphere">>},
                          {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    Geometry2 = {struct, [{<<"type">>, <<"sphere">>},
                          {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}},
                          {<<"transforms">>, [
                                              {struct, [{<<"type">>, <<"translate">>},
                                                        {<<"parameters">>, {struct, [{<<"dx">>, 0.5},
                                                                                     {<<"dy">>, 0.5},
                                                                                     {<<"dz">>, 0.5}]}}]}
                                             ]}]},  
    {ok, Id1} = node_master:create_geom(Geometry1),
    {ok, Id2} = node_master:create_geom(Geometry2),
                        
    Geometry3 = {struct, [{<<"type">>, <<"union">>},
                          {<<"children">>, [
                                            list_to_binary(Id1),
                                            list_to_binary(Id2)
                                           ]}]},

    {ok, Id3} = node_master:create_geom(Geometry3),

    node_master:serialize_geom(Id1),
    node_master:serialize_geom(Id2),
    node_master:serialize_geom(Id3),
    node_master:mesh_geom(Id1),
    node_master:serialize_brep(Id1),

    %% Restart application
    application:stop(node),
    application:start(node),
    
    %% Deserialize and load mesh
    ok = node_master:deserialize_geom(Id1),
    ok = node_master:deserialize_geom(Id2),
    ok = node_master:deserialize_geom(Id3),

    {ok, {struct, _}} = node_master:mesh_geom(Id3),
    ok.

serialize_deep_boolean(_Config) ->
    Geometry1 = {struct, [{<<"type">>, <<"sphere">>},
                          {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    Geometry2 = {struct, [{<<"type">>, <<"sphere">>},
                          {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},

    {ok, Id1} = node_master:create_geom(Geometry1),
    {ok, Id2} = node_master:create_geom(Geometry2),
                        
    Geometry3 = {struct, [{<<"type">>, <<"union">>},
                          {<<"children">>, [
                                            list_to_binary(Id1),
                                            list_to_binary(Id2)
                                           ]}]},
    {ok, Id3} = node_master:create_geom(Geometry3),

    Geometry4 = {struct, [{<<"type">>, <<"sphere">>},
                          {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    {ok, Id4} = node_master:create_geom(Geometry4),

    Geometry5 = {struct, [{<<"type">>, <<"union">>},
                          {<<"children">>, [
                                            list_to_binary(Id3),
                                            list_to_binary(Id4)
                                           ]}]},
    {ok, Id5} = node_master:create_geom(Geometry5),

    node_master:serialize_geom(Id1),
    node_master:serialize_geom(Id2),
    node_master:serialize_geom(Id3),
    node_master:serialize_geom(Id4),
    node_master:serialize_geom(Id5),

    %% Restart application
    application:stop(node),
    application:start(node),
    
    %% Deserialize and load mesh
    ok = node_master:deserialize_geom(Id1),
    ok = node_master:deserialize_geom(Id2),
    ok = node_master:deserialize_geom(Id3),
    ok = node_master:deserialize_geom(Id4),
    ok = node_master:deserialize_geom(Id5),

    {ok, {struct, _}} = node_master:mesh_geom(Id5),

    ok.
