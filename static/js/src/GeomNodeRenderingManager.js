var SS = SS || {};

SS.GeomNodeRenderingManager = function() {

    var models = {};

    this.geomDocAdd = function(geomNode) {
        if (SS.geomDoc.isRoot(geomNode)) {
            var model = new SS.GeomNodeModel({geomNode: geomNode});
            models[geomNode.id] = model;
        }
    }

    this.geomDocBeforeRemove = function(geomNode) {
        SS.selectionManager.deselectID(geomNode.id);
    }

    this.geomDocRemove = function(geomNode) {
        // Geom nodes being edited will not have a model
        if(models[geomNode.id]) {
            models[geomNode.id].destroy();
            delete models[geomNode.id];
        }

    }

    this.geomDocBeforeReplace = function(original, replacement) {
        SS.selectionManager.deselectID(original.id);
    }

    this.geomDocReplace = function(original, replacement) {
        this.geomDocRemove(original);

        if (replacement.isEditingOrTransformEditing()) {
            if (replacement.isTransformEditing()) {
                this.geomDocAdd(replacement);
            }
        } else {
            this.geomDocAdd(replacement);
        }
    }
    
    SS.geomDoc.on('add', this.geomDocAdd, this);
    SS.geomDoc.on('beforeRemove', this.geomDocBeforeRemove, this);
    SS.geomDoc.on('remove', this.geomDocRemove, this);
    SS.geomDoc.on('beforeReplace', this.geomDocBeforeReplace, this);  
    SS.geomDoc.on('replace', this.geomDocReplace, this);  

}
