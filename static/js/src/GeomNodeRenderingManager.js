var SS = SS || {};

SS.GeomNodeRenderingManager = function() {

    this.geomDocAdd = function(geomNode) {
        SS.renderGeometry(geomNode);
        if (geomNode.isEditingOrTransformEditing()) {
            SS.setOthersTransparent(geomNode);
        }
    }

    this.geomDocBeforeRemove = function(geomNode) {
        selectionManager.deselectID(geomNode.id);
    }

    this.geomDocRemove = function(geomNode) {
        SS.hideGeometry(geomNode);
        if (geomNode.isEditingOrTransformEditing()) {
            SS.restoreOpacity();
        }
    }

    this.geomDocBeforeReplace = function(original, replacement) {
        selectionManager.deselectID(original.id);
    }

    this.geomDocReplace = function(original, replacement) {
        this.geomDocRemove(original);
        this.geomDocAdd(replacement);

        if (replacement.isEditingOrTransformEditing()) {
            SS.setOthersTransparent(replacement);
        } else {
            SS.restoreOpacity();
        }
    }

    this.deselected = function(deselected) {
        for (var i in deselected) {
            var id = deselected[i];
            SS.unhighlightGeometry(geom_doc.findById(id))
        }
    }

    this.selected = function(selected) {
        for (var i in selected) {
            var id = selected[i];
            SS.highlightGeometry(geom_doc.findById(id));
        }
    }

    this.clear = function() {
        selectionManager.deselectAll();
    }
    
    geom_doc.on('add', this.geomDocAdd, this);
    geom_doc.on('beforeRemove', this.geomDocBeforeRemove, this);
    geom_doc.on('remove', this.geomDocRemove, this);
    geom_doc.on('beforeReplace', this.geomDocBeforeReplace, this);  
    geom_doc.on('replace', this.geomDocReplace, this);  

    selectionManager.on('selected', this.selected, this);
    selectionManager.on('deselected', this.deselected, this);

    command_stack.on('beforePop', this.clear, this);

}