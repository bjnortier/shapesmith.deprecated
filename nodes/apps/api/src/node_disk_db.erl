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

-module(node_disk_db).
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
    filelib:is_regular(filename(Bucket, Id)).


-spec get(bucket(), id()) -> undefined | value().
get(Bucket, Id) ->
    case file:read_file(filename(Bucket, Id)) of
	{error,enoent} -> 
	    undefined;
	{ok, Contents} ->
	    Contents
    end.
    
-spec put(bucket(), id(), value()) -> ok.
put(Bucket, Id, Value)  when is_binary(Bucket) 
			     andalso is_binary(Id) 
			     andalso is_binary(Value) ->
    Filename = filename(Bucket, Id),
    filelib:ensure_dir(Filename),
    ok = file:write_file(Filename, Value).

-spec delete(bucket(), id()) -> ok.
delete(Bucket, Id) when is_binary(Bucket) 
			andalso is_binary(Id) ->
    Filename = filename(Bucket, Id),
    filelib:ensure_dir(Filename),
    ok = file:delete(Filename).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                               Private                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

filename(Bucket, Id) ->
    {ok, DbDir} = application:get_env(node, disk_db_dir),
    filename:join(
      [filename:dirname(code:which(?MODULE)),
       "..", DbDir, binary_to_list(Bucket), binary_to_list(Id)]).

