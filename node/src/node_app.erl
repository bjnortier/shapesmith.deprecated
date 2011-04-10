%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the node application.

-module(node_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for node.
start(_Type, _StartArgs) ->
    node_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for node.
stop(_State) ->
    ok.
