-module(node_validation).
-export([geom/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

geom({struct, Props}) when is_list(Props) ->
    case lists:keyfind(<<"type">>, 1, Props) of
        {<<"type">>, GeomType} ->
            GeomResult = validate_geom_type(GeomType, Props),
	    TransformsResult = 
		    case lists:keyfind(<<"transforms">>, 1, Props) of
			{_, Transforms} ->
			    lists:map(fun transform/1, Transforms);
			false ->
			    [ok]
		    end,
	    compact_errors([GeomResult|TransformsResult]);
	_ ->
	    {error, {struct, [{<<"missing">>, <<"geometry type">>}]}}
    end;
geom(_) ->
    {error, {struct, [{<<"invalid">>, <<"geometry">>}]}}.


transform({struct, Props}) when is_list(Props) ->
    case lists:keyfind(<<"type">>, 1, Props) of
        {<<"type">>, TransformType} ->
            validate_transform_type(TransformType, Props);
        _ ->
            {error, {struct, [{<<"missing">>, <<"tranform type">>}]}}
    end;
transform(_) ->
    {error, {struct, [{<<"invalid">>, <<"transform">>}]}}.
    

-ifdef(TEST).
validate_geom_test_() ->
    [
     ?_assertEqual(ok,
		   geom({struct, [{<<"type">>, <<"sphere">>},
				  {<<"parameters">>, {struct, [
							       {<<"radius">>, 1.2}
							      ]}}]})),
     ?_assertEqual(ok,
		   geom({struct, [{<<"type">>, <<"union">>},
				  {<<"children">>, [<<"abc">>, <<"123">>]}]})),
     ?_assertEqual({error, {struct, [{<<"radius">>,<<"must be positive">>}]}},
		   geom({struct, [{<<"type">>, <<"sphere">>},
				  {<<"parameters">>, {struct, [
							       {<<"radius">>, -0.3}
							      ]}}]})),
     ?_assertEqual({error, {struct, [{<<"factor">>,<<"must be positive">>}]}},
		   geom({struct, [{<<"type">>, <<"sphere">>},
				  {<<"parameters">>, {struct, [
							       {<<"radius">>, 3}
							      ]}},
				  {<<"transforms">>,
				   [{struct, [{<<"type">>, <<"scale">>},
					      {<<"parameters">>, {struct, [{<<"x">>, 0.0},
									   {<<"y">>, 3},
									   {<<"z">>, 0.0},
									   {<<"factor">>, <<"a">>}]}}]}]}]}))

    ].
-endif.


validate_geom_type(<<"sphere">>, Props) ->
    validate_primitive(Props, [
			       {<<"radius">>, fun positive/1}
			      ]);
validate_geom_type(<<"cuboid">>, Props) ->
    validate_primitive(Props, [
			       {<<"width">>, fun positive/1},
			       {<<"depth">>, fun positive/1},
			       {<<"height">>, fun positive/1}
			      ]);
validate_geom_type(<<"cylinder">>, Props) ->
    validate_primitive(Props, [
			       {<<"radius">>, fun positive/1},
			       {<<"height">>, fun positive/1}
			      ]);
validate_geom_type(<<"cone">>, Props) ->
    validate_primitive(Props, [
			       {[<<"top_radius">>, <<"bottom_radius">>], fun one_zero_one_positive/1},
			       {<<"height">>, fun positive/1}
			      ]);
validate_geom_type(<<"wedge">>, Props) ->
    validate_primitive(Props, [
			       {<<"x1">>, fun positive/1},
			       {<<"x2">>, fun positive_or_zero/1},
			       {<<"y">>, fun positive/1},
			       {<<"z">>, fun positive/1}
			      ]);
validate_geom_type(<<"torus">>, Props) ->
    validate_primitive(Props, [
			       {<<"r1">>, fun positive/1},
			       {<<"r2">>, fun positive/1}
			      ]);
validate_geom_type(<<"union">>, Props) ->
    validate_boolean(Props);
validate_geom_type(<<"subtract">>, Props) ->
    validate_boolean(Props);
validate_geom_type(<<"intersect">>, Props) ->
    validate_boolean(Props);
validate_geom_type(_, _) ->
    {error, {struct, [{<<"invalid">>, <<"unknown geometry type">>}]}}.

validate_primitive(Props, Specs) ->
    case lists:keyfind(<<"parameters">>, 1, Props) of
	false ->
	    {error, {struct, [{<<"missing">>, <<"parameters">>}]}};
	{_, Parameters} ->
	    validate_parameters(Parameters, Specs)
    end.

validate_boolean(Props) ->
    case lists:keyfind(<<"children">>, 1, Props) of
	false ->
	    {error, {struct, [{<<"missing">>, <<"children">>}]}};
	{_, Children} when is_list(Children) ->
	    ok;
	_ ->
	    {error, {struct, [{<<"invalid">>, <<"children">>}]}}
    end.

    
-ifdef(TEST).
validate_geom_type_test_() ->
    [
     ?_assertEqual(
        ok, 
        validate_geom_type(<<"sphere">>, [{<<"parameters">>, 
					   {struct, [{<<"radius">>, 0.1}]}}])),
     ?_assertEqual(
        {error, {struct, [{<<"radius">>, <<"not found">>}]}}, 
        validate_geom_type(<<"sphere">>, [{<<"parameters">>, 
					   {struct, []}}])),
     ?_assertEqual(
        {error, {struct, [{<<"radius">>, <<"must be positive">>}]}}, 
        validate_geom_type(<<"sphere">>, [{<<"parameters">>, 
					   {struct, [{<<"radius">>, -4}]}}])),
     ?_assertEqual(
        {error, {struct, [{<<"width">>, <<"must be positive">>},
			  {<<"height">>, <<"must be positive">>}]}}, 
        validate_geom_type(<<"cuboid">>, [{<<"parameters">>,
					   {struct, [{<<"height">>, -4},
						     {<<"depth">>, 3.1},
						     {<<"width">>, -0.1}
						    ]}}]))
    ].
-endif.

validate_transform_type(<<"translate">>, Props) ->
    validate_transform_parameters(Props, [{<<"dx">>, fun number/1},
					  {<<"dy">>, fun number/1},
					  {<<"dz">>, fun number/1}
					 ]);
validate_transform_type(<<"scale">>, Props) ->
    validate_transform_parameters(Props, [{<<"x">>, fun number/1},
					  {<<"y">>, fun number/1},
					  {<<"z">>, fun number/1},
					  {<<"factor">>, fun positive/1}
					 ]);
validate_transform_type(<<"mirror">>, Props) ->
    validate_transform_parameters(Props, [
					  {<<"px">>, fun number/1},
					  {<<"py">>, fun number/1},
					  {<<"pz">>, fun number/1},
					  {<<"vx">>, fun number/1},
					  {<<"vy">>, fun number/1},
					  {<<"vz">>, fun number/1}
					 ]);
validate_transform_type(<<"rotate">>, Props) ->
    validate_transform_parameters(Props, [
					  {<<"px">>, fun number/1},
					  {<<"py">>, fun number/1},
					  {<<"pz">>, fun number/1},
					  {<<"vx">>, fun number/1},
					  {<<"vy">>, fun number/1},
					  {<<"vz">>, fun number/1},
					  {<<"angle">>, fun positive/1}
					 ]);
validate_transform_type(<<"copy_translate">>, Props) ->
    validate_transform_parameters(Props, [{<<"dx">>, fun number/1},
					  {<<"dy">>, fun number/1},
					  {<<"dz">>, fun number/1},
					  {<<"n">>, fun positive_integer/1}
					 ]);
validate_transform_type(<<"copy_rotate">>, Props) ->
    validate_transform_parameters(Props, [
					  {<<"px">>, fun number/1},
					  {<<"py">>, fun number/1},
					  {<<"pz">>, fun number/1},
					  {<<"vx">>, fun number/1},
					  {<<"vy">>, fun number/1},
					  {<<"vz">>, fun number/1},
					  {<<"angle">>, fun positive/1},
					  {<<"n">>, fun positive_integer/1}
					 ]);
validate_transform_type(<<"copy_mirror">>, Props) ->
    validate_transform_parameters(Props, [
					  {<<"px">>, fun number/1},
					  {<<"py">>, fun number/1},
					  {<<"pz">>, fun number/1},
					  {<<"vx">>, fun number/1},
					  {<<"vy">>, fun number/1},
					  {<<"vz">>, fun number/1}
					 ]);
validate_transform_type(_, _) ->
    {error, {struct, [{<<"invalid">>, <<"transform">>}]}}.

validate_transform_parameters(Props, Specs) ->
    case lists:keyfind(<<"parameters">>, 1, Props) of
	false ->
	    {error, {struct, [{<<"missing">>, <<"parameters">>}]}};
	{_, Parameters} ->
	    validate_parameters(Parameters, Specs)
    end.

    


-ifdef(TEST).
validate_transform_type_test_() ->
    [
     ?_assertEqual(
        ok, 
        validate_transform_type(<<"scale">>, [{<<"parameters">>, 
						{struct, [{<<"x">>, 0.1},
							  {<<"y">>, 0},
							  {<<"z">>, -5},
							  {<<"factor">>, 2}
							 ]}}])),
     ?_assertEqual(
        {error, {struct, [{<<"y">>, <<"must be a number">>},
			  {<<"factor">>, <<"must be positive">>}
			 ]}}, 
        validate_transform_type(<<"scale">>, [{<<"parameters">>, 
						{struct, [{<<"x">>, 0.1},
							  {<<"y">>, <<"a">>},
							  {<<"z">>, -5},
							  {<<"factor">>, -0.1}
							 ]}}]))
    ].
-endif.

validate_parameters(Parameters, Specs) ->
    Errors = lists:foldr(fun(Spec, Acc) ->
                                 case validate_spec(Parameters, Spec) of
                                     ok ->
                                         Acc;
				     Errors when is_list(Errors) ->
					 Errors ++ Acc;
                                     Error ->
                                         [Error|Acc]
                                 end
                         end,
                         [],
                         Specs),
    case Errors of
        [] ->
            ok;
        _ ->
            {error, {struct, Errors}}
    end.

-ifdef(TEST).
validate_parameters_test_() ->
    [
     ?_assertEqual(
        ok, 
        validate_parameters({struct, [{<<"a">>, 0.1},
                                      {<<"b">>, 3}]},
                            [
                             {<<"a">>, fun positive/1},
                             {<<"b">>, fun positive/1}
                            ])),

     ?_assertEqual(
        {error, {struct, [{<<"b">>, <<"must be positive">>}]}}, 
        validate_parameters({struct, [{<<"a">>, 0.1},
                                      {<<"b">>, -3}]},
                            [
                             {<<"a">>, fun positive/1},
                             {<<"b">>, fun positive/1}
                            ])),
     
     ?_assertEqual(
        {error, {struct, [{<<"a">>, <<"must be positive">>},
			  {<<"b">>, <<"must be positive">>}]}},
        validate_parameters({struct, [{<<"b">>, -0.6},
                                      {<<"a">>, -20}]},
                            [
                             {<<"a">>, fun positive/1},
                             {<<"b">>, fun positive/1}
                            ])),
     ?_assertEqual(
        {error, {struct, [{<<"a">>, <<"one must be positive">>},
			  {<<"b">>, <<"one must be positive">>}]}},
        validate_parameters({struct, [{<<"b">>, 0},
                                      {<<"a">>, 0}]},
                            [
                             {[<<"a">>, <<"b">>], fun one_zero_one_positive/1}
                            ]))
    ].
-endif.

validate_spec({struct, ParameterProps}, {Fields, CheckFn}) when is_list(Fields) ->
    Values = lists:foldr(fun(Field, Acc) ->
				 case lists:keyfind(Field, 1, ParameterProps) of
				     false -> 
					 Acc;
				     {_, Value} ->
					 [Value|Acc]
				 end
			 end,
			 [],
			 Fields),
    NumberOfFields = length(Fields),
    case length(Values) of
	NumberOfFields ->
	    case CheckFn(Values) of
                ok ->
                    ok;
                {error, Reason} ->
		    [{Field, Reason} || Field <- Fields]
            end; 
	_ ->
	    {Fields, <<"not found">>}
    end;
validate_spec({struct, ParameterProps}, {Field, CheckFn}) ->
    case lists:keyfind(Field, 1, ParameterProps) of
        false ->
            {Field, <<"not found">>};
        {_, Value} ->
            case CheckFn(Value) of
                ok ->
                    ok;
                {error, Reason} ->
                    {Field, Reason}
            end;
        _ ->
            ok
    end.

positive(Value) when is_integer(Value) andalso Value > 0 ->
    ok;
positive(Value) when is_float(Value) andalso Value > 0 ->
    ok;
positive(_) ->
    {error, <<"must be positive">>}.

positive_or_zero(Value) when is_integer(Value) andalso Value >= 0 ->
    ok;
positive_or_zero(Value) when is_float(Value) andalso Value >= 0 ->
    ok;
positive_or_zero(_) ->
    {error, <<"must be positive">>}.


positive_integer(Value) when is_integer(Value) andalso Value > 0 ->
    ok;
positive_integer(_) ->
    {error, <<"must be a positive integer">>}.

number(Value) when is_integer(Value) orelse is_float(Value) ->
    ok;
number(_) ->
    {error, <<"must be a number">>}.

one_zero_one_positive([A, B]) when A > 0 andalso B >= 0 ->
    ok;
one_zero_one_positive([A, B]) when B > 0 andalso A >= 0 ->
    ok;
one_zero_one_positive(_) ->
    {error, <<"one must be positive">>}.


-ifdef(TEST).
validate_spec_test_() ->
    [
     ?_assertEqual(ok, 
                   validate_spec({struct, [{<<"a">>, 0.1}]}, {<<"a">>, fun positive/1})),
     ?_assertEqual({<<"a">>, <<"must be positive">>}, 
                   validate_spec({struct, [{<<"a">>, 0}]}, {<<"a">>, fun positive/1})),
     ?_assertEqual({<<"a">>, <<"not found">>}, 
                   validate_spec({struct, []}, {<<"a">>, fun positive/1})),
     ?_assertEqual({<<"a">>, <<"must be a positive integer">>}, 
                   validate_spec({struct, [{<<"a">>, 0}]}, {<<"a">>, fun positive_integer/1})),
     ?_assertEqual({<<"a">>, <<"must be a number">>}, 
                   validate_spec({struct, [{<<"a">>, <<"x">>}]}, {<<"a">>, fun number/1})),
     ?_assertEqual([{<<"a">>, <<"one must be positive">>},
		    {<<"b">>, <<"one must be positive">>}],
                   validate_spec({struct, [{<<"a">>, 0},
					   {<<"b">>, 0}]}, 
				 {[<<"a">>,<<"b">>], fun one_zero_one_positive/1}))
     
    ].
-endif.


compact_errors(Results) ->
    CompactedProps = lists:foldl(fun(ok, Acc) ->
					 Acc;
				    ({error, {struct, Props}}, Acc) ->
					 Acc ++ Props
				 end,
				 [],
				 Results),
    case CompactedProps of
	[] ->
	    ok;
	_ ->
	    {error, {struct, CompactedProps}}
    end.

    
-ifdef(TEST).
compact_errors_test_() ->
    [
     ?_assertEqual(ok, compact_errors([ok, ok, ok, ok])),
     ?_assertEqual({error, {struct, [x, y]}}, 
		   compact_errors([ok, {error, {struct, [x]}}, ok, {error, {struct, [y]}}]))
    ].
-endif.

    

