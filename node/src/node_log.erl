-module(node_log).
-export([info/2]).

info(Format, Args) ->
    io:format(Format, Args).
