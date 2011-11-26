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
-export([hash_json/1, hex/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

hash_json(Geometry) ->
    hex(crypto:sha(jiffy:encode(order_json(Geometry)))).

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

order_json({Props}) when is_list(Props) ->
    Sorted = lists:sort(fun({A, _}, {B, _}) ->
				A < B
			end,
			Props),
    {lists:map(fun({Key, Value}) ->
		      {Key, order_json(Value)}
	      end,
	      Sorted)};
order_json(X) ->
    X.


-ifdef(TEST).

hex_test_() ->
    [
     ?_assertEqual("00", hex_octet(0)),
     ?_assertEqual("20", hex_octet(32)),
     ?_assertEqual("ff", hex_octet(255)),
     ?_assertEqual("00", hex(<<0>>)),
     ?_assertEqual("10", hex(<<16>>)),
     ?_assertEqual("1000", hex(<<16, 0>>)),
     ?_assertEqual("4c17", hex(<<76, 23>>))
    ].

ordering_test_() ->
    [
     ?_assertEqual({[{<<"a">>, 2}, {<<"b">>, 1}]}, 
		   order_json({[{<<"b">>, 1}, {<<"a">>, 2}]})),
     ?_assertEqual({[{<<"a">>, 2}, {<<"b">>, {[{<<"x">>, 2}, {<<"z">>, 3}]} }]}, 
		   order_json({[{<<"a">>, 2}, {<<"b">>, {[{<<"z">>, 3}, {<<"x">>, 2}]} }]})),

     ?_assertEqual(hash_json({[{<<"a">>, 1.0}, {<<"b">>, -123}]}),
		   hash_json({[{<<"b">>, -123}, {<<"a">>, 1.0}]}))
    ].

-endif.

