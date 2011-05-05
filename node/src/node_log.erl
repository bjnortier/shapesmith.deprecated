-module(node_log).
-export([info/1, info/2]).

info(Format) ->
    io:format(Format, []).

info(Format, Args) ->
    io:format(Format, Args).


