var SS = SS || {};

if(!Detector.webgl){
    Detector.addGetWebGLMessage({id: 'webgl-detector'});
    throw('no WebGL');
}

requirejs([
        "src/geometry/PlaneGeometry2",
        "src/geometry/WedgeGeometry",
        "src/geometry/EllipseGeometry",
        "src/geometry/PipeGeometry",
        "src/Spinner",
        "src/Messages",
        "src/Stack",
        "src/Command",
        "src/GeomNode",
        "src/WorkplaneNode",
        "src/GeomDocument",
        "src/schemas",
        "src/materials",
        "src/geomnode_rendering",
        "src/dom_rendering",
        "src/UIState",
        "src/TreeView",
        "src/workplane",
        "src/popupmenu",
        "src/SceneSelector",
        "src/SceneView",
        "src/GeomNodeRenderingManager",
        "src/creators/NodeModel",
    ], function() {
        requirejs([
            "src/creators/Creator",
        ], function() {
            requirejs([
                "src/creators/WorkplaneView",
                "src/creators/WorkplaneEditor",
                "src/creators/DimensionArrowsView",
                "src/creators/Transformer",
            ], function() {
                requirejs([
                    "src/creators/CuboidCreator",
                    "src/creators/SphereCreator",
                    "src/creators/CylinderCreator",
                    "src/creators/ConeCreator",
                    "src/creators/WedgeCreator",
                    "src/creators/TorusCreator",
                    "src/creators/Ellipse2DCreator",
                    "src/creators/Rectangle2DCreator",
                    "src/creators/Triangle2DCreator",
                    "src/creators/Text2DCreator",
                    "src/creators/PrismCreator",
                    "src/creators/RevolveCreator",
                    "src/creators/BezierCreator",
                    "src/creators/Ellipse1DCreator",
                    "src/creators/PolylineCreator",
                    "src/creators/FilletCreator",
                    "src/creators/TranslateTransformer",
                    "src/creators/ScaleTransformer",
                    "src/creators/RotateTransformer",
                    "src/creators/AxisMirrorTransformCreator",
                    "src/creators/PlaneMirrorTransformCreator",
                    "src/TransformerManager",
                    "src/utils",
                    "src/csgutils",
                    "src/SelectionManager",
                    "src/toolbars",
                    "src/RestAPI",
                ], function() {

                    var container = document.getElementById('scene');
                    SS.sceneView = new SS.SceneView(container);
                    SS.sceneView.animate();

                    SS.selectionManager = new SS.SelectionManager();
                    SS.commandStack = new SS.CommandStack();

                    SS.treeView = new SS.TreeView();

                    SS.transformerManager = new SS.TransformerManager();
                    SS.geomNodeRenderingManager = new SS.GeomNodeRenderingManager();

                    (function() {
                        var setUpdateScene = function() {
                            SS.sceneView.updateScene = true;
                        };
                        geom_doc.on('add', setUpdateScene);
                        geom_doc.on('remove', setUpdateScene);
                        geom_doc.on('replace', setUpdateScene);
                    })();

                    SS.workplaneModel = new SS.WorkplaneDisplayModel();

                    $(document).ready(function() {
                        SS.loadCommitFromStateOrParam(undefined);
                    });

                });
            });
        });
    });


