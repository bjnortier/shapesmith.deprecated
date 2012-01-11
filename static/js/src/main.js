var selectionManager = new SelectionManager();
Observable.makeObservable(selectionManager);

SS.selectionManager = selectionManager;
var command_stack = new CommandStack(SS);

var geom_doc = new GeomDocument();
var treeView = new TreeView();

geom_doc.addListener(function(event) {
    treeView.geomDocUpdated(event);
});

selectionManager.addListener(function(event) {
    treeView.selectionUpdated(event);
});

geom_doc.addListener(function(event) {
    sceneView.geomDocUpdated(event);
});
selectionManager.addListener(function(event) {
    sceneView.selectionUpdated(event);
});

