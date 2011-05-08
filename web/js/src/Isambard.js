var command_stack = new CommandStack();

command_stack.showSpinner = showSpinner;
command_stack.hideSpinner = hideSpinner;
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
});
selectionManager.addListener(function(event) {
    sceneView.selectionUpdated(event);
});

selectionManager.addListener(function(event) {
    sceneView.selectionUpdated(event);
});
selectionManager.addListener(function(event) {
    
    if (selectionManager.size() == 1) {
        $('#action_stl').unbind('click');
        var pattern = /^\/geom\/(.*)$/;
        var id = selectionManager.selected()[0].match(pattern)[1];
        $('#action_stl').attr('href', '/stl/' + id); 
    } else {
        $('#action_stl').unbind('click');
        $('#action_stl').click(function() {
            alert("select one object"); 
            return false;
        });
    }
});


$(document).ready(function() {
    var docId = $.getQueryParam("docid");
    if (docId == undefined) {
        alert('no document defined!');
        return;
    }
    load(docId);
    resetAspectRatio();
});
