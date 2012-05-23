var SS = SS || {};

var selectionManager = new SelectionManager();

SS.selectionManager = selectionManager;
var command_stack = new CommandStack(SS);

var treeView = new TreeView();

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

