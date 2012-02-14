var SS = SS || {};

SS.UIStates = {UNDEFINED : 0, EDITING : 1};

SS.UIState = function() {

    this.state = SS.UIStates.UNDEFINED;
    
    this.geomDocAdd = function(node) {
        if (node.isEditingOrTransformEditing()) {
            this.state = SS.UIStates.EDITING;
        }
    }

    this.geomDocRemove = function(node) {
        if (node.isEditingOrTransformEditing()) {
            this.state = SS.UIStates.UNDEFINED;
        }
    }

    this.geomDocReplace = function(original, replacement) {
        this.state = replacement.isEditingOrTransformEditing() ? SS.UIStates.EDITING : SS.UIStates.UNDEFINED;
    }

    geom_doc.on('add', this.geomDocAdd, this);
    geom_doc.on('remove', this.geomDocRemove, this);
    geom_doc.on('replace', this.geomDocReplace, this);
    
}
