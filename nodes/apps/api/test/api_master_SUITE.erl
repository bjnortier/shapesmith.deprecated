-module(api_master_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
         create_simple, 
         create_boolean,
	 parallel_workers,
         no_worker
	].

init_per_suite(Config) ->
    ok = api_deps:start_without_api(),
    Config.

end_per_suite(_Config) ->
    ok = api_deps:stop_without_api().

init_per_testcase(Testcase, Config) ->
    ok = application:load(api),
    ok = application:set_env(api, port, 8001),
    ok = application:set_env(api, db_module, api_mem_db),
    case Testcase of 
        no_worker -> 
            ok = application:set_env(api, worker_wait_max_secs, 0);
        _ ->
            ok
    end,
    ok = application:start(api),
    {ok, _} = api_mem_db:start_link(),
    Config.

end_per_testcase(_Testcase, _Config) ->
    stopped = api_mem_db:stop(),
    ok = application:stop(api),
    ok = application:unload(api),
    ok.

create_simple(_Config) ->
    Geometry = {[{<<"type">>, <<"sphere">>},
		 {<<"origin">>, {[{<<"x">>, 0},
				  {<<"y">>, 0},
				  {<<"z">>, 0}]}},
		 {<<"parameters">>, {[{<<"r">>, 1.0}]}}]},
    {ok, SHA} = api_master:create_geom("bjnortier", "iphonedock", Geometry),
    {ok, {_}} = api_master:mesh_geom("bjnortier", "iphonedock", SHA),
    {ok, Stl} = api_master:stl("bjnortier", "iphonedock", SHA),
    true = is_binary(Stl),
    ok.

no_worker(_Config) ->
    GeomFn = fun(R) -> 
                     {[{<<"type">>,<<"sphere">>}, 
                       {<<"origin">>, {[{<<"x">>, 0},
                                        {<<"y">>, 0},
                                        {<<"z">>, 0}]}},
                       {<<"parameters">>,{[{<<"r">>, R}]}}]}
             end,
    _Pid1 = worker_master_pool:get_worker(0),
    _Pid2 = worker_master_pool:get_worker(0),
    {error, no_worker_available} = api_master:create_geom("bjnortier", "iphonedock", GeomFn(11)),
    ok.

create_boolean(_Config) ->
    Geometry1 = {[{<<"type">>, <<"sphere">>},
		  {<<"origin">>, {[{<<"x">>, 0},
				   {<<"y">>, 0},
				   {<<"z">>, 0}]}},
		  {<<"parameters">>, {[{<<"r">>, 10.0}]}}]},
    Geometry2 = {[{<<"type">>, <<"sphere">>},
		  {<<"origin">>, {[{<<"x">>, 0},
				   {<<"y">>, 0},
				   {<<"z">>, 0}]}},
		  {<<"parameters">>, {[{<<"r">>, 5.0}]}},
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

    {ok, SHA1} = api_master:create_geom("bjnortier", "iphonedock", Geometry1),
    {ok, SHA2} = api_master:create_geom("bjnortier", "iphonedock", Geometry2),

    Geometry3 = {[{<<"type">>, <<"union">>},
		  {<<"children">>, [
				    list_to_binary(SHA1),
				    list_to_binary(SHA2)
				   ]}]},
    {ok, SHA3} = api_master:create_geom("bjnortier", "iphonedock", Geometry3),

    {ok, _} = api_master:mesh_geom("bjnortier", "iphonedock", SHA3),
    {ok, Stl} = api_master:stl("bjnortier", "iphonedock", SHA3),
    true = is_binary(Stl),

    ok.

parallel_workers(_Config) ->
    Results = pmap(fun(R) -> 
			   Geom =  {[{<<"type">>,<<"sphere">>}, 
				     {<<"origin">>, {[{<<"x">>, 0},
						      {<<"y">>, 0},
						      {<<"z">>, 0}]}},
				     {<<"parameters">>,{[{<<"r">>, R}]}}]},
			   api_master:create_geom("bjnortier", "iphonedock", Geom) end, 
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


