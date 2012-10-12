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

-module(api_modeller_resource).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([
	 init/1, 
         allowed_methods/2,
	 is_authorized/2,
	 content_types_provided/2,
	 resource_exists/2,
	 provide_content/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
    {ok, {}}.

allowed_methods(ReqData, Context) -> 
    {['GET'], ReqData, Context}.

is_authorized(ReqData, Context) ->
    api_resource:redirect_to_signin_if_not_authorized(ReqData, Context).

content_types_provided(ReqData, Context) ->
    {[{"text/html", provide_content}], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {true, ReqData, Context}.

provide_content(ReqData, Context) ->
    User = wrq:path_info(user, ReqData),
    Design = wrq:path_info(design, ReqData),
    Commit = wrq:get_qs_value("commit", ReqData),
    {ok, Host} = application:get_env(api, host),
    {ok, ScriptsEnv} = application:get_env(api, js_scripts_environment),
    WalrusContext =  [{username, User},
		      {design, Design},
		      {commit, Commit},
		      {host, Host},
              {appscripts, appscripts(ScriptsEnv)}
		     ],

    {ok, AuthModule} = application:get_env(api, auth_module),
    WalrusContext1 = AuthModule:add_session_walrus_ctx(User, WalrusContext),

    Rendered = api_walrus:render_template(api_views_modeller, WalrusContext1),
    {Rendered, api_resource:prevent_caching(ReqData), Context}.

appscripts(production) ->
    [
        [{file, "shapesmith.js"}]
    ];
appscripts(_) ->
    [
        [{file, "src/geometry/PlaneGeometry2.js"}],
        [{file, "src/geometry/WedgeGeometry.js"}],
        [{file, "src/geometry/EllipseGeometry.js"}],
        [{file, "src/geometry/PipeGeometry.js"}],
        [{file, "src/Spinner.js"}],
        [{file, "src/Messages.js"}],
        [{file, "src/Stack.js"}],
        [{file, "src/Command.js"}],
        [{file, "src/GeomNode.js"}],
        [{file, "src/WorkplaneNode.js"}],
        [{file, "src/GeomDocument.js"}],
        [{file, "src/schemas.js"}],
        [{file, "src/materials.js"}],
        [{file, "src/geomnode_rendering.js"}],
        [{file, "src/dom_rendering.js"}],
        [{file, "src/UIState.js"}],
        [{file, "src/TreeView.js"}],
        [{file, "src/workplane.js"}],
        [{file, "src/popupmenu.js"}],
        [{file, "src/SceneSelector.js"}],
        [{file, "src/SceneView.js"}],
        [{file, "src/GeomNodeRenderingManager.js"}],
        [{file, "src/creators/NodeModel.js"}],
        [{file, "src/creators/Creator.js"}],
        [{file, "src/creators/WorkplaneView.js"}],
        [{file, "src/creators/WorkplaneEditor.js"}],
        [{file, "src/creators/DimensionArrowsView.js"}],
        [{file, "src/creators/Transformer.js"}],
        [{file, "src/creators/CuboidCreator.js"}],
        [{file, "src/creators/SphereCreator.js"}],
        [{file, "src/creators/CylinderCreator.js"}],
        [{file, "src/creators/ConeCreator.js"}],
        [{file, "src/creators/WedgeCreator.js"}],
        [{file, "src/creators/TorusCreator.js"}],
        [{file, "src/creators/Ellipse2DCreator.js"}],
        [{file, "src/creators/Rectangle2DCreator.js"}],
        [{file, "src/creators/Triangle2DCreator.js"}],
        [{file, "src/creators/Text2DCreator.js"}],
        [{file, "src/creators/PrismCreator.js"}],
        [{file, "src/creators/RevolveCreator.js"}],
        [{file, "src/creators/BezierCreator.js"}],
        [{file, "src/creators/Ellipse1DCreator.js"}],
        [{file, "src/creators/PolylineCreator.js"}],
        [{file, "src/creators/FilletCreator.js"}],
        [{file, "src/creators/TranslateTransformer.js"}],
        [{file, "src/creators/ScaleTransformer.js"}],
        [{file, "src/creators/RotateTransformer.js"}],
        [{file, "src/creators/AxisMirrorTransformCreator.js"}],
        [{file, "src/creators/PlaneMirrorTransformCreator.js"}],
        [{file, "src/TransformerManager.js"}],
        [{file, "src/utils.js"}],
        [{file, "src/csgutils.js"}],
        [{file, "src/SelectionManager.js"}],
        [{file, "src/toolbars.js"}],
        [{file, "src/RestAPI.js"}]
    ].

