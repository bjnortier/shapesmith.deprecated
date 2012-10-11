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

-module(api_validation).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([geom/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

geom({Props}) when is_list(Props) ->
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
	    {error, {[{<<"missing">>, <<"geometry type">>}]}}
    end;
geom(_) ->
    {error, {[{<<"invalid">>, <<"geometry">>}]}}.


transform({Props}) when is_list(Props) ->
    case lists:keyfind(<<"type">>, 1, Props) of
        {<<"type">>, TransformType} ->
            X = validate_transform_type(TransformType, Props),
	    io:format("~p~n", [X]),
	    X;
        _ ->
            {error, {[{<<"missing">>, <<"tranform type">>}]}}
    end;
transform(_) ->
    {error, {[{<<"invalid">>, <<"transform">>}]}}.


-ifdef(TEST).
validate_geom_test_() ->
    Origin = {[{<<"x">>, 0},
	       {<<"y">>, 0},
	       {<<"z">>, 0}]},
    [
     ?_assertEqual(ok,
		   geom({[{<<"type">>, <<"sphere">>},
			  {<<"origin">>, Origin},
			  {<<"parameters">>, {[
					       {<<"r">>, 1.2}
					      ]}}]})),
     ?_assertEqual(ok,
		   geom({[{<<"type">>, <<"union">>},
			  {<<"children">>, [<<"abc">>, <<"123">>]}]})),
     ?_assertEqual({error, {[{<<"r">>,<<"must be positive">>}]}},
		   geom({[{<<"type">>, <<"sphere">>},
			  {<<"origin">>, Origin},
			  {<<"parameters">>, {[
					       {<<"r">>, -0.3}
					      ]}}]})),
     ?_assertEqual({error, {[{<<"factor">>,<<"must be positive">>}]}},
		   geom({[{<<"type">>, <<"sphere">>},
			  {<<"origin">>, Origin},
			  {<<"parameters">>, {[
					       {<<"r">>, 3}
					      ]}},
			  {<<"transforms">>,
			   [{[{<<"type">>, <<"scale">>},
			      {<<"origin">>, Origin},
			      {<<"parameters">>, {[{<<"x">>, 0.0},
						   {<<"y">>, 3},
						   {<<"z">>, 0.0},
						   {<<"factor">>, <<"a">>}]}}]}]}]})),
     ?_assertEqual({error, {[{<<"vertices">>, <<"three vertices required">>}]}},
		   geom({[{<<"type">>, <<"triangle2d">>},
			  {<<"origin">>, Origin},
                          {<<"parameters">>, {[{<<"vertices">>, [{[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]}]}]}}
                         ]})),
     ?_assertEqual({error, {[{<<"vertices">>, <<"at least 2 vertices required">>}]}},
		   geom({[{<<"type">>, <<"polyline">>},
			  {<<"origin">>, Origin},
                          {<<"parameters">>, {[{<<"vertices">>, [{[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]}]}]}}
                         ]})),
     ?_assertEqual(ok,
		   geom({[{<<"type">>, <<"polyline">>},
			  {<<"origin">>, Origin},
                          {<<"parameters">>, {[{<<"vertices">>, [{[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]},
                                                                 {[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 1}]}
                                                                ]}
                                              ]}}
                         ]})),

     ?_assertEqual({error, {[{<<"vertices">>, <<"all coordinates must be numbers">>}]}},
		   geom({[{<<"type">>, <<"triangle2d">>},
			  {<<"origin">>, Origin},
                          {<<"parameters">>, {[{<<"vertices">>, [{[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]},
                                                                 {[{<<"u">>, 0}, {<<"v">>, <<"a">>}, {<<"w">>, 0}]},
                                                                 {[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]}
                                                                ]}]}}
                         ]})),
     ?_assertEqual(ok,
		   geom({[{<<"type">>, <<"triangle2d">>},
			  {<<"origin">>, Origin},
                          {<<"parameters">>, {[{<<"vertices">>, [{[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]},
                                                                 {[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]},
                                                                 {[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]}
                                                                ]}]}}
                         ]})),

     ?_assertEqual({error, {[{<<"vertices">>, <<"four vertices required">>}]}},
		   geom({[{<<"type">>, <<"bezier">>},
			  {<<"origin">>, Origin},
                          {<<"parameters">>, {[{<<"vertices">>, [{[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]}]}]}}
                         ]})),
     ?_assertEqual({error, {[{<<"vertices">>, <<"all coordinates must be numbers">>}]}},
		   geom({[{<<"type">>, <<"bezier">>},
			  {<<"origin">>, Origin},
                          {<<"parameters">>, {[{<<"vertices">>, [{[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]},
                                                                 {[{<<"u">>, 0}, {<<"v">>, <<"a">>}, {<<"w">>, 0}]},
                                                                 {[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]},
                                                                 {[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 1}]}
                                                                ]}]}}
                         ]})),
     ?_assertEqual(ok,
		   geom({[{<<"type">>, <<"bezier">>},
			  {<<"origin">>, Origin},
                          {<<"parameters">>, {[{<<"vertices">>, [{[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]},
                                                                 {[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]},
                                                                 {[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]},
                                                                 {[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]}
                                                                ]}]}}
                         ]})),

     ?_assertEqual({error, {[{<<"children">>, <<"only one child allowed">>}]}},
		   geom({[{<<"type">>, <<"prism">>},
			  {<<"origin">>, Origin},
                          {<<"parameters">>, {[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}]}},
                          {<<"children">>, [<<"abc">>, <<"123">>]}]})),
     ?_assertEqual({error, {[{<<"children">>, <<"only one child allowed">>}]}},
		   geom({[{<<"type">>, <<"revolve">>},
			  {<<"origin">>, Origin},
                          {<<"parameters">>, {[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}, {<<"angle">>, 360}]}},
                          {<<"children">>, [<<"abc">>, <<"123">>]}]})),
     ?_assertEqual({error, {[{<<"angle">>, <<"must be non-zero">>}]}},
		   geom({[{<<"type">>, <<"revolve">>},
			  {<<"origin">>, Origin},
                          {<<"parameters">>, {[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}, {<<"angle">>, <<"foo">>}]}},
                          {<<"children">>, [<<"abc">>]}
                         ]})),
     ?_assertEqual({error, {[{<<"angle">>, <<"must be non-zero">>}]}},
		   geom({[{<<"type">>, <<"revolve">>},
			  {<<"origin">>, Origin},
                          {<<"parameters">>, {[{<<"u">>, 0}, {<<"v">>, 0}, {<<"w">>, 0}, {<<"angle">>, 0}]}},
                          {<<"children">>, [<<"abc">>]}
                         ]})),
     ?_assertEqual(ok, 
		   geom({[{<<"type">>, <<"fillet">>},
                          {<<"parameters">>, {[{<<"r">>, 5}]}},
                          {<<"children">>, [<<"abc">>]}]})),
     
     %% Import STL
     ?_assertEqual({error, {[{<<"missing">>, <<"contents">>}]}},
		   geom({[{<<"type">>, <<"import_stl">>}]})),
     ?_assertEqual({error, {[{<<"contents">>, <<"'contents' must be base64 encoded">>}]}},
        	   geom({[{<<"type">>, <<"import_stl">>},
                          {<<"contents">>, <<"a">>}]})),
     ?_assertEqual(ok, 
        	   geom({[{<<"type">>, <<"import_stl">>},
                          {<<"contents">>, <<"Zm9v">>}]}))

    ].
-endif.


validate_geom_type(<<"sphere">>, Props) ->
    validate_primitive(Props, [
			       {<<"r">>, fun positive/1}
			      ]);
validate_geom_type(<<"cuboid">>, Props) ->
    validate_primitive(Props, [
			       {<<"u">>, fun not_zero/1},
			       {<<"v">>, fun not_zero/1},
			       {<<"w">>, fun not_zero/1}
			      ]);
validate_geom_type(<<"cylinder">>, Props) ->
    validate_primitive(Props, [
			       {<<"r">>, fun positive/1},
			       {<<"h">>, fun not_zero/1}
			      ]);
validate_geom_type(<<"cone">>, Props) ->
    validate_primitive(Props, [
			       {[<<"r1">>, <<"r2">>], fun one_zero_one_positive/1},
			       {[<<"r1">>, <<"r2">>], fun not_equal/1},
			       {<<"h">>, fun not_zero/1}
			      ]);
validate_geom_type(<<"wedge">>, Props) ->
    validate_primitive(Props, [
			       {<<"u1">>, fun positive/1},
			       {<<"u2">>, fun positive_or_zero/1},
			       {<<"v">>, fun positive/1},
			       {<<"w">>, fun not_zero/1}
			      ]);
validate_geom_type(<<"torus">>, Props) ->
    validate_primitive(Props, [
			       {<<"r1">>, fun positive/1},
			       {<<"r2">>, fun positive/1}
			      ]);
validate_geom_type(<<"ellipse2d">>, Props) ->
    validate_primitive(Props, [
			       {<<"r1">>, fun positive/1},
			       {<<"r2">>, fun positive/1}
			      ]);
validate_geom_type(<<"rectangle2d">>, Props) ->
    validate_primitive(Props, [
			       {<<"u">>, fun not_zero/1},
			       {<<"v">>, fun not_zero/1}
			      ]);
validate_geom_type(<<"ellipse1d">>, Props) ->
    validate_primitive(Props, [
			       {<<"r1">>, fun positive/1},
			       {<<"r2">>, fun positive/1},
                               {<<"from_angle">>, fun number/1},
			       {<<"to_angle">>, fun number/1}
			      ]);
validate_geom_type(<<"text2d">>, Props) ->
    validate_primitive(Props, [
			       {<<"text">>, fun non_empty_string/1},
                               {<<"font">>, fun supported_fonts/1}
			      ]);
validate_geom_type(<<"triangle2d">>, Props) ->
    validate_primitive(Props, [{<<"vertices">>, fun triangle_vertices/1}]);
validate_geom_type(<<"polyline">>, Props) ->
    validate_primitive(Props, [{<<"vertices">>, fun polyline_vertices/1}]);
validate_geom_type(<<"bezier">>, Props) ->
    validate_primitive(Props, [{<<"vertices">>, fun bezier_vertices/1}]);
validate_geom_type(<<"prism">>, Props) ->
    case lists:keyfind(<<"children">>, 1, Props) of
	false ->
	    {error, {[{<<"missing">>, <<"children">>}]}};
	{_, [_Child]} ->
            validate_primitive(Props, [{<<"u">>, fun number/1},
                                       {<<"v">>, fun number/1},
                                       {<<"w">>, fun number/1}
                                      ]);
	_ ->
	    {error, {[{<<"children">>, <<"only one child allowed">>}]}}
    end;
validate_geom_type(<<"revolve">>, Props) ->
    case lists:keyfind(<<"children">>, 1, Props) of
	false ->
	    {error, {[{<<"missing">>, <<"children">>}]}};
	{_, [_Child]} ->
            validate_primitive(Props, [{<<"u">>, fun number/1},
                                       {<<"v">>, fun number/1},
                                       {<<"w">>, fun number/1},
                                       {<<"angle">>, fun not_zero/1}
                                      ]);
	_ ->
	    {error, {[{<<"children">>, <<"only one child allowed">>}]}}
    end;
validate_geom_type(<<"make_face">>, Props) ->
    case lists:keyfind(<<"children">>, 1, Props) of
	false ->
	    {error, {[{<<"missing">>, <<"children">>}]}};
	{_, []} ->
	    {error, {[{<<"children">>, <<"cannot be empty">>}]}};
	{_, Children} when is_list(Children) ->
            ok
    end;
validate_geom_type(<<"make_solid">>, Props) ->
    case lists:keyfind(<<"children">>, 1, Props) of
	false ->
	    {error, {[{<<"missing">>, <<"children">>}]}};
	{_, []} ->
	    {error, {[{<<"children">>, <<"cannot be empty">>}]}};
	{_, Children} when is_list(Children) ->
            ok
    end;
validate_geom_type(<<"fillet">>, Props) ->
    case lists:keyfind(<<"children">>, 1, Props) of
	false ->
	    {error, {[{<<"missing">>, <<"children">>}]}};
	{_, [_Child]} ->
            case lists:keyfind(<<"parameters">>, 1, Props) of
                false ->
                    {error, {[{<<"missing">>, <<"parameters">>}]}};
                {_, Parameters} ->
                    validate_parameters(Parameters, [{<<"r">>, fun positive/1}])
            end;
	_ ->
	    {error, {[{<<"children">>, <<"only one child allowed">>}]}}
    end;
validate_geom_type(<<"loft">>, Props) ->
    case lists:keyfind(<<"children">>, 1, Props) of
	false ->
	    {error, {[{<<"missing">>, <<"children">>}]}};
	{_, Children} when is_list(Children) ->
            ok;
	_ ->
	    {error, {[{<<"children">>, <<"only one child allowed">>}]}}
    end;
validate_geom_type(<<"union">>, Props) ->
    validate_boolean(Props);
validate_geom_type(<<"subtract">>, Props) ->
    validate_boolean(Props);
validate_geom_type(<<"intersect">>, Props) ->
    validate_boolean(Props);
validate_geom_type(<<"import_stl">>, Props) ->
    case lists:keyfind(<<"contents">>, 1, Props) of
        false ->
            {error, {[{<<"missing">>, <<"contents">>}]}};
        {_, Contents} ->
            try
                base64:decode(Contents),
                ok
            catch 
                _:_ ->
                    {error, {[{<<"contents">>, <<"'contents' must be base64 encoded">>}]}}
            end
    end;
validate_geom_type(_, _) ->
    {error, {[{<<"invalid">>, <<"unknown geometry type">>}]}}.

validate_primitive(Props, Specs) ->
    case lists:keyfind(<<"origin">>, 1, Props) of
	false ->
	    {error, {[{<<"missing">>, <<"origin">>}]}};
	{_, Origin} ->
	    case validate_origin(Origin) of
		ok ->
		    case lists:keyfind(<<"parameters">>, 1, Props) of
			false ->
			    {error, {[{<<"missing">>, <<"parameters">>}]}};
			{_, Parameters} ->
			    validate_parameters(Parameters, Specs)
		    end;
		Error ->
		    Error
	    end

    end.

validate_origin(Origin) ->
    validate_parameters(Origin, [
				 {<<"x">>, fun number/1},
				 {<<"y">>, fun number/1},
				 {<<"z">>, fun number/1}
				]).


validate_boolean(Props) ->
    case lists:keyfind(<<"children">>, 1, Props) of
	false ->
	    {error, {[{<<"missing">>, <<"children">>}]}};
	{_, Children} when is_list(Children) ->
	    ok;
	_ ->
	    {error, {[{<<"invalid">>, <<"children">>}]}}
    end.


-ifdef(TEST).
validate_geom_type_test_() ->
    Origin = {[{<<"x">>, 0},
	       {<<"y">>, 0},
	       {<<"z">>, 0}]},
    [
     ?_assertEqual(
        ok, 
        validate_geom_type(<<"sphere">>, [{<<"origin">>, Origin},
					  {<<"parameters">>, 
					   {[{<<"r">>, 0.1}]}}])),
     ?_assertEqual(
        {error, {[{<<"r">>, <<"not found">>}]}}, 
        validate_geom_type(<<"sphere">>, [{<<"origin">>, Origin},
					  {<<"parameters">>, 
					   {[]}}])),
     ?_assertEqual(
        {error, {[{<<"r">>, <<"must be positive">>}]}}, 
        validate_geom_type(<<"sphere">>, [{<<"origin">>, Origin},
					  {<<"parameters">>, 
					   {[{<<"r">>, -4}]}}])),
     ?_assertEqual(
        {error, {[{<<"u">>, <<"must be non-zero">>},
		  {<<"w">>, <<"must be non-zero">>}]}}, 
        validate_geom_type(<<"cuboid">>, [{<<"origin">>, Origin},
					  {<<"parameters">>,
					   {[{<<"u">>, 0},
					     {<<"v">>, 3.1},
					     {<<"w">>, 0}
					    ]}}])),
     ?_assertEqual(
        {error, {[{<<"text">>, <<"must be a non-empty string">>}]}}, 
        validate_geom_type(<<"text2d">>, [{<<"origin">>, Origin},
					  {<<"parameters">>, 
					   {[{<<"text">>, 0.1}, {<<"font">>, <<"DroidSerif">>}]}}])),
     ?_assertEqual(
        {error, {[{<<"text">>, <<"must be a non-empty string">>}]}}, 
        validate_geom_type(<<"text2d">>, [{<<"origin">>, Origin},
					  {<<"parameters">>, 
					   {[{<<"text">>, <<>>}, {<<"font">>, <<"Inconsolata">>}]}}])),
     ?_assertEqual(
        {error, {[{<<"font">>, <<"must be one of 'DroidSerif', 'Inconsolata', 'Lobster' or 'OpenSans'">>}]}}, 
        validate_geom_type(<<"text2d">>, [{<<"origin">>, Origin},
					  {<<"parameters">>, 
					   {[{<<"text">>, <<"some text">>},
                                             {<<"font">>, <<"Arial">>}]}}])),
     ?_assertEqual(
        ok, 
        validate_geom_type(<<"text2d">>, [{<<"origin">>, Origin},
					  {<<"parameters">>, 
					   {[{<<"text">>, <<"some text">>}, {<<"font">>, <<"Lobster">>}]}}]))

    ].
-endif.


validate_transform_type(<<"translate">>, Props) ->
    validate_primitive(Props, [{<<"u">>, fun number/1},
			       {<<"v">>, fun number/1},
			       {<<"w">>, fun number/1},
			       {<<"n">>, fun positive_or_zero_integer/1}
			      ]);
validate_transform_type(<<"scale">>, Props) ->
    validate_primitive(Props, [
			       {<<"factor">>, fun positive/1}
			      ]);
validate_transform_type(<<"rotate">>, Props) ->
    validate_primitive(Props, [
			       {<<"u">>, fun number/1},
			       {<<"v">>, fun number/1},
			       {<<"w">>, fun number/1},
			       {<<"angle">>, fun number/1},
			       {<<"n">>, fun positive_or_zero_integer/1}
			      ]);
validate_transform_type(<<"mirror">>, Props) ->
    validate_primitive(Props, [
			       {<<"u">>, fun number/1},
			       {<<"v">>, fun number/1},
			       {<<"w">>, fun number/1},
			       {<<"n">>, fun zero_or_one_integer/1}
			      ]);
validate_transform_type(<<"axis_mirror">>, Props) ->
    validate_primitive(Props, [
			       {<<"u">>, fun number/1},
			       {<<"v">>, fun number/1},
			       {<<"w">>, fun number/1},
			       {<<"n">>, fun zero_or_one_integer/1}
			      ]);
validate_transform_type(<<"plane_mirror">>, Props) ->
    validate_primitive(Props, [
			       {<<"u">>, fun number/1},
			       {<<"v">>, fun number/1},
			       {<<"w">>, fun number/1},
			       {<<"n">>, fun zero_or_one_integer/1}
			      ]);

validate_transform_type(_, _) ->
    {error, {[{<<"invalid">>, <<"transform">>}]}}.





-ifdef(TEST).
validate_transform_type_test_() ->
    Origin = {[{<<"x">>, 0},
	       {<<"y">>, 0},
	       {<<"z">>, 0}]},
    [
     ?_assertEqual(
        ok, 
        validate_transform_type(<<"scale">>, [{<<"origin">>, Origin},
					      {<<"parameters">>, 
					       {[{<<"x">>, 0.1},
						 {<<"y">>, 0},
						 {<<"z">>, -5},
						 {<<"factor">>, 2}
						]}}])),
     ?_assertEqual(
        {error, {[{<<"factor">>, <<"must be positive">>}
		 ]}}, 
        validate_transform_type(<<"scale">>, [{<<"origin">>, Origin},
					      {<<"parameters">>, 
					       {[{<<"x">>, 0.1},
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
            {error, {Errors}}
    end.

-ifdef(TEST).
validate_parameters_test_() ->
    [
     ?_assertEqual(
        ok, 
        validate_parameters({[{<<"a">>, 0.1},
			      {<<"b">>, 3}]},
                            [
                             {<<"a">>, fun positive/1},
                             {<<"b">>, fun positive/1}
                            ])),

     ?_assertEqual(
        {error, {[{<<"b">>, <<"must be positive">>}]}}, 
        validate_parameters({[{<<"a">>, 0.1},
			      {<<"b">>, -3}]},
                            [
                             {<<"a">>, fun positive/1},
                             {<<"b">>, fun positive/1}
                            ])),

     ?_assertEqual(
        {error, {[{<<"a">>, <<"must be positive">>},
		  {<<"b">>, <<"must be positive">>}]}},
        validate_parameters({[{<<"b">>, -0.6},
			      {<<"a">>, -20}]},
                            [
                             {<<"a">>, fun positive/1},
                             {<<"b">>, fun positive/1}
                            ])),
     ?_assertEqual(
        {error, {[{<<"a">>, <<"one must be positive">>},
		  {<<"b">>, <<"one must be positive">>}]}},
        validate_parameters({[{<<"b">>, 0},
			      {<<"a">>, 0}]},
                            [
                             {[<<"a">>, <<"b">>], fun one_zero_one_positive/1}
                            ]))
    ].
-endif.


validate_spec({ParameterProps}, {Fields, CheckFn}) when is_list(Fields) ->
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
validate_spec({ParameterProps}, {Field, CheckFn}) ->
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

non_empty_string(<<>>) ->
    {error, <<"must be a non-empty string">>};
non_empty_string(Value) when is_binary(Value) ->
    ok;
non_empty_string(_) ->
    {error, <<"must be a non-empty string">>}.

supported_fonts(<<"DroidSerif">>) ->
    ok;
supported_fonts(<<"Inconsolata">>) ->
    ok;
supported_fonts(<<"Lobster">>) ->    
    ok;
supported_fonts(<<"OpenSans">>) ->
    ok;
supported_fonts(_) ->
    {error, <<"must be one of 'DroidSerif', 'Inconsolata', 'Lobster' or 'OpenSans'">>}.
    

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
    {error, <<"must be >= 0">>}.

positive_or_zero_integer(Value) when is_integer(Value) andalso Value >= 0 ->
    ok;
positive_or_zero_integer(_) ->
    {error, <<"must be and integer and >= 0">>}.

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

not_equal([A, B]) when A =:= B ->
    {error, <<"can't be equal">>};
not_equal(_) ->
    ok.

not_zero(Value) when is_integer(Value) andalso Value =/= 0 ->
    ok;
not_zero(Value) when is_float(Value) andalso Value =/= 0 ->
    ok;
not_zero(_) ->
    {error, <<"must be non-zero">>}.

zero_or_one_integer(Value) when is_integer(Value) andalso Value =:= 0 ->
    ok;
zero_or_one_integer(Value) when is_integer(Value) andalso Value =:= 1 ->
    ok;
zero_or_one_integer(_) ->
    {error, <<"must be 0 or one">>}.

triangle_vertices([{V1}, {V2}, {V3}]) ->
    case {vertex(V1), vertex(V2), vertex(V3)} of
        {ok, ok, ok} ->
            ok;
        _ ->
            {error, <<"all coordinates must be numbers">>}
    end;
triangle_vertices(_) ->
    {error, <<"three vertices required">>}.

polyline_vertices(Vertices) when is_list(Vertices) andalso length(Vertices) > 1 ->
    lists:foldl(fun({Vertex}, ok) ->
                        vertex(Vertex);
                   (_, {error, Err}) ->
                        {error, Err}
                end,
                ok,
                Vertices);
polyline_vertices(_) ->
    {error, <<"at least 2 vertices required">>}.

bezier_vertices([{V1}, {V2}, {V3}, {V4}]) ->
    case {vertex(V1), vertex(V2), vertex(V3), vertex(V4)} of
        {ok, ok, ok, ok} ->
            ok;
        _ ->
            {error, <<"all coordinates must be numbers">>}
    end;
bezier_vertices(_) ->
    {error, <<"four vertices required">>}.

vertex(Vertex) ->
    case {lists:keyfind(<<"u">>, 1, Vertex), lists:keyfind(<<"v">>, 1, Vertex), lists:keyfind(<<"w">>, 1, Vertex)}of
        {{_, U}, {_, V}, {_, W}} ->
            case {number(U), number(V), number(W)} of
                {ok, ok, ok} ->
                    ok;
                _ ->
                    {error, <<"all coordinates must be numbers">>}
            end;
        _ ->
            {error, <<"all coordinates must be numbers">>}
    end.




-ifdef(TEST).
validate_spec_test_() ->
    [
     ?_assertEqual(ok, 
                   validate_spec({[{<<"a">>, 0.1}]}, {<<"a">>, fun positive/1})),
     ?_assertEqual({<<"a">>, <<"must be positive">>}, 
                   validate_spec({[{<<"a">>, 0}]}, {<<"a">>, fun positive/1})),
     ?_assertEqual({<<"a">>, <<"not found">>}, 
                   validate_spec({[]}, {<<"a">>, fun positive/1})),
     ?_assertEqual({<<"a">>, <<"must be a number">>}, 
                   validate_spec({[{<<"a">>, <<"x">>}]}, {<<"a">>, fun number/1})),
     ?_assertEqual([{<<"a">>, <<"one must be positive">>},
		    {<<"b">>, <<"one must be positive">>}],
                   validate_spec({[{<<"a">>, 0},
				   {<<"b">>, 0}]}, 
				 {[<<"a">>,<<"b">>], fun one_zero_one_positive/1})),
     ?_assertEqual([{<<"a">>, <<"can't be equal">>},
		    {<<"b">>, <<"can't be equal">>}],
                   validate_spec({[{<<"a">>, 1},
				   {<<"b">>, 1}]}, 
				 {[<<"a">>,<<"b">>], fun not_equal/1}))


    ].
-endif.


compact_errors(Results) ->
    CompactedProps = lists:foldl(fun(ok, Acc) ->
					 Acc;
				    ({error, {Props}}, Acc) ->
					 Acc ++ Props
				 end,
				 [],
				 Results),
    case CompactedProps of
	[] ->
	    ok;
	_ ->
	    {error, {CompactedProps}}
    end.


-ifdef(TEST).
compact_errors_test_() ->
    [
     ?_assertEqual(ok, compact_errors([ok, ok, ok, ok])),
     ?_assertEqual({error, {[x, y]}}, 
		   compact_errors([ok, {error, {[x]}}, ok, {error, {[y]}}]))
    ].
-endif.

    

