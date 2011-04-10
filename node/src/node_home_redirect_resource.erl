-module(node_home_redirect_resource).
-export([init/1, resource_exists/2, moved_temporarily/2, previously_existed/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

moved_temporarily(ReqData, Context) ->
    {{true, "/index.html"}, ReqData, Context}.

previously_existed(ReqData, Context) -> {true, ReqData, Context}.

resource_exists(ReqData, Context) -> {false, ReqData, Context}.
