var SS = SS || {};

SS.UIStates = {UNDEFINED : 0, EDITING : 1};

SS.UIState = function() {

    this.state = SS.UIStates.UNDEFINED;
    
    var isEditing = function(node) {
        var editingTransform = _.pluck(node.transforms, "editing").indexOf(true) >= 0;
        return node.editing || editingTransform;
    }
    
    this.geomDocAdd = function(node) {
        if (isEditing(node)) {
            this.state = SS.UIStates.EDITING;
        }
    }

    this.geomDocRemove = function(node) {
        if (isEditing(node)) {
            this.state = SS.UIStates.UNDEFINED;
        }
    }

    this.geomDocReplace = function(original, replacement) {
        this.state = isEditing(replacement) ? SS.UIStates.EDITING : SS.UIStates.UNDEFINED;
    }

    geom_doc.on('add', this.geomDocAdd, this);
    geom_doc.on('remove', this.geomDocRemove, this);
    geom_doc.on('replace', this.geomDocReplace, this);
    
}
