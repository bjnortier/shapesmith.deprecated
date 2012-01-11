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
    Config.

end_per_suite(_Config) ->
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
    stopped = node_mem_db:stop(),
    ok = application:stop(node),
    ok = application:unload(node),
    ok.

create_simple(_Config) ->
    Geometry = {[{<<"type">>, <<"sphere">>},
		 {<<"origin">>, {[{<<"x">>, 0},
				  {<<"y">>, 0},
				  {<<"z">>, 0}]}},
		 {<<"parameters">>, {[{<<"r">>, 1.0}]}}]},
    {ok, SHA} = node_master:create_geom("bjnortier", "iphonedock", Geometry),
    {ok, {_}} = node_master:mesh_geom("bjnortier", "iphonedock", SHA),
    {ok, Stl} = node_master:stl("bjnortier", "iphonedock", SHA),
    true = is_binary(Stl),
    ok.

timeout(_Config) ->
    Geometry = {[{<<"type">>, <<"sphere">>},
		 {<<"parameters">>, {[{<<"radius">>, 1.0}]}}]},
    {error, _} = node_master:create_geom("bjnortier", "iphonedock", Geometry),
    ok.

create_boolean(_Config) ->
    Geometry1 = {[{<<"type">>, <<"sphere">>},
		  {<<"origin">>, {[{<<"x">>, 0},
				   {<<"y">>, 0},
				   {<<"z">>, 0}]}},
		  {<<"parameters">>, {[{<<"r">>, 1.0}]}}]},
    Geometry2 = {[{<<"type">>, <<"sphere">>},
		  {<<"origin">>, {[{<<"x">>, 0},
				   {<<"y">>, 0},
				   {<<"z">>, 0}]}},
		  {<<"parameters">>, {[{<<"r">>, 1.0}]}},
		  {<<"transforms">>, [
				      {[{<<"type">>, <<"translate">>},
					{<<"origin">>, {[{<<"x">>, 0},
							 {<<"y">>, 0},
							 {<<"z">>, 0}]}},
					{<<"parameters">>, {[{<<"u">>, 0.5},
							     {<<"v">>, 0.5},
							     {<<"w">>, 0.5},
							     {<<"n">>, 0}
							    ]}}]}
				     ]}]},

    {ok, SHA1} = node_master:create_geom("bjnortier", "iphonedock", Geometry1),
    {ok, SHA2} = node_master:create_geom("bjnortier", "iphonedock", Geometry2),

    Geometry3 = {[{<<"type">>, <<"union">>},
		  {<<"children">>, [
				    list_to_binary(SHA1),
				    list_to_binary(SHA2)
				   ]}]},
    {ok, SHA3} = node_master:create_geom("bjnortier", "iphonedock", Geometry3),

    {ok, _} = node_master:mesh_geom("bjnortier", "iphonedock", SHA3),
    {ok, Stl} = node_master:stl("bjnortier", "iphonedock", SHA3),
    true = is_binary(Stl),

    ok.

parallel_workers(_Config) ->
    Results = pmap(fun(R) -> 
			   Geom =  {[{<<"type">>,<<"sphere">>}, 
				     {<<"origin">>, {[{<<"x">>, 0},
						      {<<"y">>, 0},
						      {<<"z">>, 0}]}},
				     {<<"parameters">>,{[{<<"r">>, R}]}}]},
			   node_master:create_geom("bjnortier", "iphonedock", Geom) end, 
		   lists:seq(1,10)),
    lists:map(fun(Result) ->
		      {ok, _SHA} = Result
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


