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
    hex(crypto:sha(jiffy:encode(Geometry))).

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

ordering_test_() ->
    [
     ?_assertEqual(hash_geometry({[{<<"a">>, 1.0}, {<<"b">>, -123}]})
		   hash_geometry({[{<<"b">>, -123}, {<<"a">>, 1.0}]})),
     ?_assertEqual(true, false) %% Add recursive testcase
    ].

-endif.

