-module(node_master_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
         create_simple,
         create_boolean,
	 timeout,
	 parallel_workers
	].

init_per_suite(Config) ->
    ok = application:start(inets),
    Config.

end_per_suite(_Config) ->
    application:stop(node),
    ok.
    

init_per_testcase(Testcase, Config) ->
    ok = application:load(node),
    ok = application:set_env(node, port, 8001),
    ok = application:set_env(node, db_module, node_mem_db),
    case Testcase of 
	timeout -> 
	    ok = application:set_env(node, worker_max_time, 0);
	_ ->
	    ok
    end,
    ok = node:start(),
    {ok, _} = node_mem_db:start_link(),
    Config.

end_per_testcase(_Testcase, _Config) ->
    node_mem_db:stop(),
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

timeout(_Config) ->
    Geometry = {struct, [{<<"type">>, <<"sphere">>},
                         {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    {error, _} = node_master:create_geom(Geometry),
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

parallel_workers(_Config) ->
    Results = pmap(fun(R) -> 
			   Geom =  {struct,[{<<"type">>,<<"sphere">>}, 
					    {<<"parameters">>,{struct,[{<<"radius">>, R}]}}]},
			   node_master:create_geom(Geom) end, 
		   lists:seq(1,10)),
    lists:map(fun(Result) ->
		      {ok, _Id} = Result
	      end,
	      Results).
    

pmap(F, L) ->
    S = self(),
    Ref = erlang:make_ref(),
    Pids = lists:map(fun(I) ->
			     spawn(fun() -> do_f(S, Ref, F, I) end)
		     end, 
		     L),
    gather(Pids, Ref).

do_f(Parent, Ref, F, I) ->
    Parent ! {self(), Ref, (catch F(I))}.

gather([Pid| Rest], Ref) ->
    receive 
	{Pid, Ref, Ret} ->
	    [Ret|gather(Rest, Ref)]
    end;
gather([], _) ->
    [].


