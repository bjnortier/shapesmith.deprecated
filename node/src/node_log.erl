-module(node_log).
-export([info/1, info/2]).
-export([error/1, error/2]).
-compile({no_auto_import,[error/2]}).

info(Format) ->
    info(Format, []).

info(Format, Args) ->
    io:format("[INFO] " ++ Format, Args).

error(Format) ->
    error(Format, []).

error(Format, Args) ->
    io:format("[ERROR] " ++ Format, Args).



