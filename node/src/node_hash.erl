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

-module(node_hash).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([hash_geometry/1, hex/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

hash_geometry(Geometry) ->
    Filtered = hashable(Geometry),
    hex(crypto:sha(term_to_binary(Filtered))).

hex(Binary) when is_binary(Binary) ->
    lists:flatten([hex_octet(X) || X <- binary_to_list(Binary)]).
          

hex_octet(N) when N =< 9 ->
    [$0, $0 + N];
hex_octet(N) when N > 15 ->
    hex_nibble(N bsr 4) ++ hex_nibble(N band 15);
hex_octet(N) ->
    [$0, N - 10 + $a].

hex_nibble(N) when N =< 9 ->
    [$0 + N];
hex_nibble(N) ->
    [N - 10 + $a].


-ifdef(TEST).

hex_test_() ->
    [
     ?_assertEqual("00", hex_octet(0)),
     ?_assertEqual("20", hex_octet(32)),
     ?_assertEqual("ff", hex_octet(255)),
     ?_assertEqual("00", hex_binary(<<0>>)),
     ?_assertEqual("10", hex_binary(<<16>>)),
     ?_assertEqual("1000", hex_binary(<<16, 0>>)),
     ?_assertEqual("4c17", hex_binary(<<76, 23>>))
    ].

-endif.

hashable({struct, Props}) ->
    {<<"type">>, Type} = lists:keyfind(<<"type">>, 1, Props),
    OptionalOrigin = case lists:keyfind(<<"origin">>, 1, Props) of
			 false -> 
			     [];
			 {_, Orgn} -> 
			     [{<<"origin">>, Orgn}]
		     end,
    Parameters = case lists:keyfind(<<"parameters">>, 1, Props) of
                     false -> 
                         {struct, []};
                     {_, Params} -> 
                         Params
                 end,
    FilteredChildren = case lists:keyfind(<<"children">>, 1, Props) of
                           false -> [];
                           {_, Children} -> 
                               [hashable(X) || X <- Children]
                       end,
    Transforms = case  lists:keyfind(<<"transforms">>, 1, Props) of
                     false -> [];
                     {_, Transfrms} -> 
                         Transfrms
                 end,
    {struct, [{<<"type">>, Type}] ++ OptionalOrigin ++ [{<<"parameters">>, Parameters},
							{<<"children">>, FilteredChildren},
							{<<"transforms">>, Transforms}]}.

-ifdef(TEST).

hashable_simple_test_() ->
    Geom1 = {struct, [{<<"type">>, <<"sphere">>},
                      {<<"id">>, <<"abc">>},
                      {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    Geom2 = {struct, [{<<"type">>, <<"sphere">>},
                      {<<"id">>, <<"def">>},
		      {<<"origin">>, {struct, [{<<"x">>, 0}, {<<"y">>, 0}, {<<"z">>, 0}]}},
                      {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    [
     ?_assertEqual({struct, [{<<"type">>, <<"sphere">>},
                             {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}},
                             {<<"children">>, []},
                             {<<"transforms">>, []}]},
                   hashable(Geom1)),
     ?_assertEqual({struct, [{<<"type">>, <<"sphere">>},
			     {<<"origin">>, {struct, [{<<"x">>, 0}, {<<"y">>, 0}, {<<"z">>, 0}]}},
			     {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}},
                             {<<"children">>, []},
                             {<<"transforms">>, []}]},
                   hashable(Geom2))
    ].

hashable_with_transforms_test_() ->
    Geom1 = {struct, [{<<"type">>, <<"sphere">>},
                      {<<"id">>, <<"abc">>},
                      {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}},
                      {<<"transforms">>, [{struct, [{<<"type">>, <<"translate">>}]}]}
                     ]},
    [
     ?_assertEqual({struct, [{<<"type">>, <<"sphere">>},
                             {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}},
                             {<<"children">>, []},
                             {<<"transforms">>, [{struct, [{<<"type">>, <<"translate">>}]}]}]},
                   hashable(Geom1))
    ].

hashable_boolean_test_() ->
    Geom1 = {struct, [{<<"type">>, <<"union">>},
                      {<<"id">>, <<"abc">>},
                      {<<"children">>, [
                                        {struct, [{<<"type">>, <<"cuboid">>},
                                                  {<<"id">>, <<"def">>},
                                                  {<<"parameters">>, {struct, [{<<"x">>, 1.0}]}}]},
                                        {struct, [{<<"type">>, <<"sphere">>},
                                                  {<<"id">>, <<"rst">>},
                                                  {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]}
                                       ]}]},


    [
     ?_assertEqual(
        {struct, [{<<"type">>, <<"union">>},
                  {<<"parameters">>, {struct, []}},
                  {<<"children">>, [
                                    {struct, [{<<"type">>, <<"cuboid">>},
                                              {<<"parameters">>, {struct, [{<<"x">>, 1.0}]}},
                                              {<<"children">>, []},
                                              {<<"transforms">>, []}]},
                                    {struct, [{<<"type">>, <<"sphere">>},
                                              {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}},
                                              {<<"children">>, []},
                                              {<<"transforms">>, []}]}
                                   ]},
                  {<<"transforms">>, []}
                 ]},
        hashable(Geom1))
    ].


-endif.


