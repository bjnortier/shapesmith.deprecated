#!/usr/bin/env sh
echo 'minifying csg...'
java -jar ~/development/closure-compiler/compiler-latest/compiler.jar lib/csg.js --js_output_file lib/csg-min.js --jscomp_off=internetExplorerChecks --language_in=ECMASCRIPT5

echo 'minifying /src...'
java -jar ~/development/closure-compiler/compiler-latest/compiler.jar \
src/geometry/PlaneGeometry2.js \
src/geometry/WedgeGeometry.js \
src/geometry/EllipseGeometry.js \
src/geometry/PipeGeometry.js \
src/Spinner.js \
src/Messages.js \
src/Stack.js \
src/Command.js \
src/GeomNode.js \
src/WorkplaneNode.js \
src/GeomDocument.js \
src/schemas.js \
src/materials.js \
src/geomnode_rendering.js \
src/dom_rendering.js \
src/UIState.js \
src/TreeView.js \
src/workplane.js \
src/popupmenu.js \
src/SceneSelector.js \
src/SceneView.js \
src/GeomNodeRenderingManager.js \
src/geomNode/GeomNodeMV.js \
src/creators/NodeModel.js \
src/creators/Creator.js \
src/creators/WorkplaneView.js \
src/creators/WorkplaneEditor.js \
src/creators/DimensionArrowsView.js \
src/creators/Transformer.js \
src/creators/CuboidCreator.js \
src/creators/SphereCreator.js \
src/creators/CylinderCreator.js \
src/creators/ConeCreator.js \
src/creators/WedgeCreator.js \
src/creators/TorusCreator.js \
src/creators/Ellipse2DCreator.js \
src/creators/Rectangle2DCreator.js \
src/creators/Triangle2DCreator.js \
src/creators/Text2DCreator.js \
src/creators/PrismCreator.js \
src/creators/RevolveCreator.js \
src/creators/BezierCreator.js \
src/creators/Ellipse1DCreator.js \
src/creators/PolylineCreator.js \
src/creators/FilletCreator.js \
src/creators/TranslateTransformer.js \
src/creators/ScaleTransformer.js \
src/creators/RotateTransformer.js \
src/creators/AxisMirrorTransformCreator.js \
src/creators/PlaneMirrorTransformCreator.js \
src/TransformerManager.js \
src/utils.js \
src/csgutils.js \
src/SelectionManager.js \
src/toolbars.js \
src/RestAPI.js \
--js_output_file shapesmith.js --jscomp_off=internetExplorerChecks --language_in=ECMASCRIPT5

echo 'done.'