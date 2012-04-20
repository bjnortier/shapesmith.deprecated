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

-module(node_riak_db).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([exists/2, get/2, put/3, delete/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-type bucket() :: binary().
-type id() :: binary().
-type value() :: binary().

-spec exists(bucket(), id()) -> true | false.
exists(Bucket, Id) ->
    get(Bucket, Id) =/= undefined.

-spec get(bucket(), id()) -> undefined | value().
get(Bucket, Id) when is_binary(Bucket) andalso is_binary(Id)->
    Client = get_client(),
    case riakc_pb_socket:get(Client, Bucket, Id) of
	{error, notfound} ->
	    riakc_pb_socket:stop(Client),
	    undefined;
	{ok, Obj} ->
	    Result = riakc_obj:get_value(Obj),
	    riakc_pb_socket:stop(Client),
	    Result
    end.
    
-spec put(bucket(), id(), value()) -> {error, notfound} | ok.
put(Bucket, Id, Value)  when is_binary(Bucket) andalso is_binary(Id) andalso is_binary(Value) ->
    Client = get_client(),
    case riakc_pb_socket:get(Client, Bucket, Id) of
	{error, notfound} ->
	    Obj = riakc_obj:new(Bucket, Id, Value),
	    ok = riakc_pb_socket:put(Client, Obj),
	    riakc_pb_socket:stop(Client),
	    ok;
	{ok, ObjA} ->
	    ObjB = riakc_obj:update_value(ObjA, Value),
	    ok = riakc_pb_socket:put(Client, ObjB),
	    riakc_pb_socket:stop(Client),
	    ok
    end.

-spec delete(bucket(), id()) -> undefined | value().
delete(Bucket, Id) when is_binary(Bucket) andalso is_binary(Id)->
    Client = get_client(),
    ok = riakc_pb_socket:delete(Client, Bucket, Id),
    riakc_pb_socket:stop(Client),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                               Private                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

get_client() ->
    {ok, {Host, Port}} = application:get_env(node, riak_host),
    {ok, Pid} = riakc_pb_socket:start_link(Host, Port),
    Pid.

