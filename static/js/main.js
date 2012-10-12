var SS = SS || {};

if(!Detector.webgl){
    Detector.addGetWebGLMessage({id: 'webgl-detector'});
    throw('no WebGL');
}

$(document).ready(function() {
    SS.geomDoc = new SS.GeomDocument();
    SS.UI_MOUSE_STATE = new SS.UIMouseState();
    SS.UI_EDITING_STATE = new SS.UIEditingState();

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
        SS.geomDoc.on('add', setUpdateScene);
        SS.geomDoc.on('remove', setUpdateScene);
        SS.geomDoc.on('replace', setUpdateScene);
    })();

    SS.workplaneModel = new SS.WorkplaneDisplayModel();


    /*
     * This is a pain. Chrome and Firefox have differrent behaviour for window.onpopstate().
     * Firefox will NOT generate the event when there is no session state available (e.g.
     * on a new page load). Chrome will generate and event, but Firefox will not. Hence
     * the need to $(document).ready() and the workaround variable.
     */
    if (navigator.userAgent.indexOf('Firefox') !== -1) {
        SS.loadCommitFromStateOrParam(undefined);
    }
});

       