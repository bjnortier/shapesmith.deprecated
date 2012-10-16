-module(ui_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Add the webmachine dispatch rules
    {ok, ExistingDispatchRules} = application:get_env(webmachine, dispatch_list),
    NewRules = [
            {[], ui_home_redirect_resource, []},
            {["ui", "signup"], ui_signup_resource, []},
            {["ui", "signin"], ui_signin_resource, []},
            {["ui", '*'], ui_fs_resource, [{root, "www"}]},
            {[user, "designs"], ui_designs_resource, []},
            {[user, design, "modeller"], ui_modeller_resource, []}
            
            ] ++ ExistingDispatchRules,
    ok = application:set_env(webmachine, dispatch_list, NewRules),
    
    %% Start the app
    ui_sup:start_link().

stop(_State) ->
    ok.
