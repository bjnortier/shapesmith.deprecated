var SS = SS || {};

var selectionManager = new SelectionManager();
Observable.makeObservable(selectionManager);

SS.selectionManager = selectionManager;
var command_stack = new CommandStack(SS);

var geom_doc = new GeomDocument();
var treeView = new TreeView();

SS.transformerManager = new SS.transformers.Manager();

geom_doc.addListener(function(event) {
    treeView.geomDocUpdated(event);
});
geom_doc.addListener(function(event) {
    SS.sceneView.geomDocUpdated(event);
});

selectionManager.addListener(function(event) {
    SS.sceneView.selectionUpdated(event);
});
selectionManager.addListener(function(event) {
    treeView.selectionUpdated(event);
});
selectionManager.addListener(function(event) {
    SS.transformerManager.selectionUpdated(event);
});




