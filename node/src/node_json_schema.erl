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


-module(node_json_schema).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate_json({raw, Json}, {raw, Schema}) when is_list(Json) andalso is_list(Schema) ->
    MochiJson = mochijson2:decode(Json),
    MochiSchema = mochijson2:decode(Schema),
    validate_json(MochiJson, MochiSchema);
validate_json(MochiJson, {struct, SchemaFields}) ->
    case lists:keyfind(<<"type">>, 1, SchemaFields) of
	false ->
	    {error, not_root_type};
	<<"object">> ->
	    SchemaProperties = case lists:keyfind(<<"properties">>, 1, SchemaFields) of
				   {<<"properties">>, P} ->
				       P;
				   _ ->
				       {struct, []}
			       end,
	    validate_object(MochiJson, SchemaProperties);
	{<<"type">>, Type} ->
	    validate_type(MochiJson, Type)
    end.

-ifdef(TEST).
validate_spec_test_() ->
    %% 'Geo' sample from http://json-schema.org/
    Schema = "{
	\"description\" : \"A geographical coordinate\",
	\"type\" : \"object\",
	\"properties\" : {
		\"latitude\" : { \"type\" : \"number\" },
		\"longitude\" : { \"type\" : \"number\" }
	}
}",
    [
     ?_assertEqual(ok, validate_json({raw, "{}"}, {raw, Schema})),
     ?_assertEqual(ok, validate_json({raw, "1.2"}, {raw, "{\"type\" : \"number\"}"}))
    ].

-endif.


validate_object({struct, ObjectProps}, {struct, SchemaProps}) ->
    ValidateValue = fun(Key, Value, SubSchema) ->
			    case validate_json(Value, SubSchema) of
				{error, Reason} ->
				    {error, {Key, Reason}};
				ok ->
				    ok
			    end
		    end,
    ValidateProperty = fun(Key, Value) ->
			       case lists:keyfind(Key, 1, SchemaProps) of
				   false -> 
				       ok;
				   {Key, SubSchema} ->
				       ValidateValue(Key, Value, SubSchema)
			       end
		       end,
    ResultsForEachProperty = lists:map(fun({Key, Value}) ->
					       ValidateProperty(Key, Value)
				       end,
				       ObjectProps),
    case lists:filter(fun(Result) -> Result =/= ok end,       
		      ResultsForEachProperty) of
	[] ->
	    ok;
	Errors ->
	    Errors
    end.
    

-ifdef(TEST).
validate_object_test_() ->
    EmptyPropertiesSchema = {struct, []},
    LatLonPropertiesSchema = {struct, [{<<"latitude">>, 
					{struct, [{<<"type">>, <<"number">>}]}},
				       {<<"longitude">>, 
					{struct, [{<<"type">>, <<"number">>}]}}]},
    [
     ?_assertEqual(ok, 
		   validate_object({struct, []}, EmptyPropertiesSchema)),

     %% Undefined properties are not validated as per paragraph 5.4 of the specification
     ?_assertEqual(ok, 
		   validate_object({struct, [{<<"lat">>, 10}]}, EmptyPropertiesSchema)),

     %% Neither are required
     ?_assertEqual(ok, 
		   validate_object({struct, []}, LatLonPropertiesSchema)),
     ?_assertEqual(ok, 
		   validate_object({struct, [{<<"latitude">>, 1.2}]}, LatLonPropertiesSchema)),
					      
     %% Single failure
     ?_assertEqual([{error, {<<"latitude">>, {not_a_number ,<<"foo">>}}}],
		   validate_object({struct, [{<<"latitude">>, <<"foo">>}]}, LatLonPropertiesSchema)),
     %% Multiple failures
     ?_assertEqual([{error,{<<"latitude">>,{not_a_number,<<"foo">>}}},
		    {error,{<<"latitude">>,{not_a_number,<<"bar">>}}}],
		   validate_object({struct, [{<<"latitude">>, <<"foo">>},
					     {<<"latitude">>, <<"bar">>}]}, 
				   LatLonPropertiesSchema))


    ].

-endif.


validate_type(MochiJson, Type) when is_binary(Type) ->
    validate_simple_type(MochiJson, Type);
validate_type(MochiJson, Types) when is_list(Types) ->
    validate_union_type(MochiJson, Types).
    
-ifdef(TEST).
validate_type_test_() ->
    [
     ?_assertEqual(ok, validate_type([1,7], <<"array">>)),
     ?_assertEqual(ok, validate_type([1,7], <<"array">>)),
     ?_assertEqual(ok, validate_type(5.4, [<<"array">>, <<"number">>]))
    ].
-endif.

validate_union_type(MochiJson, Types) ->
    ValidationResult = lists:foldl(fun(Type, {error, _Reason}) ->
					   validate_simple_type(MochiJson, Type);
				      (_Type, ok) ->
					   ok
				   end,
				   {error, undefined},
				   Types),
    case ValidationResult of
	ok ->
	    ok;
	{error, _Reason} ->
	    {error, {not_one_of, Types, MochiJson}}
    end.

-ifdef(TEST).
validate_union_type_test_() ->
    [
     ?_assertEqual(ok, 
		   validate_union_type(null, [<<"number">>, <<"null">>])),
     ?_assertEqual(ok, 
		   validate_union_type(3.1, [<<"number">>, <<"null">>])),

     ?_assertEqual({error, {not_one_of, [<<"number">>, null], <<"something">>}},
		   validate_union_type(<<"something">>, [<<"number">>, null]))
    ].
-endif.    


validate_simple_type(MochiJson, <<"string">>) when is_binary(MochiJson) -> ok;
validate_simple_type(MochiJson, <<"number">>) when is_float(MochiJson) orelse is_integer(MochiJson) -> ok;
validate_simple_type(MochiJson, <<"integer">>) when is_integer(MochiJson) -> ok;
validate_simple_type(true, <<"boolean">>) -> ok;
validate_simple_type(false, <<"boolean">>) -> ok;
validate_simple_type(MochiJson, <<"array">>) when is_list(MochiJson) -> ok;
validate_simple_type(_MochiJson, <<"any">>) -> ok;
validate_simple_type(null, <<"null">>) -> ok;
validate_simple_type(MochiJson, <<"object">>) when is_tuple(MochiJson) -> ok;

validate_simple_type(MochiJson, <<"string">>) ->
    {error, {not_a_string, MochiJson}};
validate_simple_type(MochiJson, <<"number">>) ->
    {error, {not_a_number, MochiJson}};
validate_simple_type(MochiJson, <<"integer">>) ->
    {error, {not_an_integer, MochiJson}};
validate_simple_type(MochiJson, <<"boolean">>) ->
    {error, {not_a_boolean, MochiJson}};
validate_simple_type(MochiJson, <<"array">>) ->
    {error, {not_an_array, MochiJson}};
validate_simple_type(MochiJson, <<"null">>) ->
    {error, {not_null, MochiJson}};
validate_simple_type(MochiJson, <<"object">>) ->
    {error, {not_an_object, MochiJson}};
validate_simple_type(_MochiJson, Type) when is_binary(Type) ->
    {error, {"unknown type", binary_to_list(Type)}};
validate_simple_type(_MochiJson, Type) ->
    {error, {"invalid schema - type not a string", Type}}.


-ifdef(TEST).
validate_simple_type_test_() ->
    [
     ?_assertEqual({error, {"unknown type", "real"}}, validate_simple_type([], <<"real">>)),
     ?_assertEqual({error, {"invalid schema - type not a string", something}}, validate_simple_type([], something)),

     ?_assertEqual({error, {not_a_string, []}}, validate_simple_type([], <<"string">>)),
     ?_assertEqual({error, {not_a_string, null}}, validate_simple_type(null, <<"string">>)),
     ?_assertEqual(ok, validate_simple_type(<<"abc">>, <<"string">>)),

     ?_assertEqual({error, {not_a_number, <<"123">>}}, validate_simple_type(<<"123">>, <<"number">>)),
     ?_assertEqual({error, {not_a_number, null}}, validate_simple_type(null, <<"number">>)),
     ?_assertEqual(ok, validate_simple_type(1.3, <<"number">>)),

     ?_assertEqual({error, {not_an_integer, 1.2}}, validate_simple_type(1.2, <<"integer">>)),
     ?_assertEqual({error, {not_an_integer, null}}, validate_simple_type(null, <<"integer">>)),
     ?_assertEqual(ok, validate_simple_type(-132, <<"integer">>)),

     ?_assertEqual({error, {not_a_boolean, [1,2]}}, validate_simple_type([1,2], <<"boolean">>)),
     ?_assertEqual({error, {not_a_boolean, null}}, validate_simple_type(null, <<"boolean">>)),
     ?_assertEqual(ok, validate_simple_type(true, <<"boolean">>)),
     ?_assertEqual(ok, validate_simple_type(false, <<"boolean">>)),

     ?_assertEqual({error, {not_an_object, <<"abc">>}}, validate_simple_type(<<"abc">>, <<"object">>)),
     ?_assertEqual({error, {not_an_object, null}}, validate_simple_type(null, <<"object">>)),
     ?_assertEqual(ok, validate_simple_type({struct, []}, <<"object">>)),

     ?_assertEqual({error, {not_an_array, <<"abc">>}}, validate_simple_type(<<"abc">>, <<"array">>)),
     ?_assertEqual({error, {not_an_array, null}}, validate_simple_type(null, <<"array">>)),
     ?_assertEqual(ok, validate_simple_type([], <<"array">>)),

     ?_assertEqual({error, {not_null, 42}}, validate_simple_type(42, <<"null">>)),
     ?_assertEqual(ok, validate_simple_type(null, <<"null">>)),

     ?_assertEqual(ok, validate_simple_type(<<"a string">>, <<"any">>)),
     ?_assertEqual(ok, validate_simple_type(5.43, <<"any">>)),
     ?_assertEqual(ok, validate_simple_type(10, <<"any">>)),
     ?_assertEqual(ok, validate_simple_type(true, <<"any">>)),
     ?_assertEqual(ok, validate_simple_type({struct,[]}, <<"any">>)),
     ?_assertEqual(ok, validate_simple_type([], <<"any">>))
    ].
-endif.   


