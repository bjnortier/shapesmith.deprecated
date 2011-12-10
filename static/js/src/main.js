var command_stack = new CommandStack(SS);

command_stack.renderErrorMessage = renderErrorMessage;
command_stack.clearMessages = clearMessages;

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
    selectionManager.geomDocUpdated(event);
});
selectionManager.addListener(function(event) {
    sceneView.selectionUpdated(event);
});

$(document).ready(function() {
});
