-module(node_riak_db).
-export([exists/2, get/2, create/2, put/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-type bucket() :: atom().
-type id() :: list().
-type value() :: term().

-spec exists(bucket(), id()) -> true | false.
exists(Bucket, Id) ->
    get(Bucket, Id) =/= undefined.

-spec get(bucket(), id()) -> undefined | value().
get(Bucket, Id) ->
    Client = get_client(),
    case riakc_pb_socket:get(Client, riak_bucket(Bucket), list_to_binary(Id)) of
	{error, notfound} ->
	    undefined;
	{ok, Obj} ->
	    binary_to_term(riakc_obj:get_value(Obj))
    end.

-spec create(bucket(), value()) -> id().
create(Bucket, Value) ->
    Id = node_uuid:uuid(),
    Obj = riakc_obj:new(riak_bucket(Bucket), list_to_binary(Id), term_to_binary(Value)),
    Client = get_client(),
    ok = riakc_pb_socket:put(Client, Obj),
    Id.
    
-spec put(bucket(), id(), value()) -> {error, notfound} | ok.
put(Bucket, Id, Value) ->
    Client = get_client(),
    case riakc_pb_socket:get(Client, riak_bucket(Bucket), list_to_binary(Id)) of
	{error, notfound} ->
	    Obj = riakc_obj:new(riak_bucket(Bucket), list_to_binary(Id), term_to_binary(Value)),
	    ok = riakc_pb_socket:put(Client, Obj);
	{ok, ObjA} ->
	    ObjB = riakc_obj:update_value(ObjA, term_to_binary(Value)),
	    ok = riakc_pb_socket:put(Client, ObjB)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                               Private                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

get_client() ->
    {ok, {Host, Port}} = application:get_env(node, riak_host),
    {ok, Pid} = riakc_pb_socket:start_link(Host, Port),
    Pid.

riak_bucket(Bucket) ->
    list_to_binary(atom_to_list(Bucket)).
