var SS = SS || {};

SS.GeomNodeRenderingManager = function() {

    var hiddenByUser = [];

    this.geomDocAdd = function(geomNode) {
        if (geom_doc.isRoot(geomNode)) {
            SS.renderGeometry(geomNode);
        }
        if (geomNode.isEditingOrTransformEditing()) {
            setOtherNonHiddenNodesTransparent(geomNode);
        }
    }

    this.geomDocBeforeRemove = function(geomNode) {
        selectionManager.deselectID(geomNode.id);
    }

    this.geomDocRemove = function(geomNode) {
        SS.hideGeometry(geomNode);

        if (geomNode.isEditingOrTransformEditing()) {
            restoreOpacityOfNonHiddenNodes();
        }

        if (hiddenByUser.indexOf(geomNode.id) !== -1) {
            hiddenByUser.splice(hiddenByUser.indexOf(geomNode.id),1);
        }
    }

    this.geomDocBeforeReplace = function(original, replacement) {
        selectionManager.deselectID(original.id);
    }

    this.geomDocReplace = function(original, replacement) {
        this.geomDocRemove(original);

        if (replacement.isEditingOrTransformEditing()) {
            if (replacement.isTransformEditing()) {
                this.geomDocAdd(replacement);
            }
            setOtherNonHiddenNodesTransparent(replacement);
        } else {
            this.geomDocAdd(replacement);
            restoreOpacityOfNonHiddenNodes();
        }
    }

    var restoreOpacityOfNonHiddenNodes = function() {
        geom_doc.rootNodes.map(function(geomNode) {
            if (hiddenByUser.indexOf(geomNode.id) === -1) {
                SS.restoreOpacity(geomNode);
            }
        });
    }

    var setOtherNonHiddenNodesTransparent = function(geomNode) {
        geom_doc.rootNodes.map(function(rootNode) {
            if (geomNode) {
                if ((geomNode.id !== rootNode.id)
                    && (hiddenByUser.indexOf(geomNode.id) === -1)) {
                    SS.setTransparent(rootNode);
                }   
            } else {
                SS.setTransparent(rootNode);
            }

        });
    }

    var uiStateChanged = function(editing) {
        if (editing) {
            setOtherNonHiddenNodesTransparent();
        } else {
            restoreOpacityOfNonHiddenNodes();
        }
    }

    this.isHiddenByUser = function(geomNode) {
        return (hiddenByUser.indexOf(geomNode.id) !== -1);
    }

    this.deselected = function(deselected) {
        for (var i in deselected) {
            var id = deselected[i];
            if (hiddenByUser.indexOf(id) === -1) {
                SS.unhighlightGeometry(geom_doc.findById(id))
            }
        }
    }

    this.selected = function(selected) {
        for (var i in selected) {
            var id = selected[i];
            var geomNode = geom_doc.findById(id);
            if (hiddenByUser.indexOf(id) === -1) {
                SS.highlightGeometry(geomNode);
            }
            setOtherNonHiddenNodesTransparent(geomNode);
        }
    }

    this.clear = function() {
        selectionManager.deselectAll();
    }

    this.setOpaque = function(id) {
        hiddenByUser.splice(hiddenByUser.indexOf(id), 1);
        SS.renderGeometry(geom_doc.findById(id));
    }

    this.setHidden = function(id) {
        hiddenByUser.push(id);
        SS.hideGeometry(geom_doc.findById(id));
    }
    
    geom_doc.on('add', this.geomDocAdd, this);
    geom_doc.on('beforeRemove', this.geomDocBeforeRemove, this);
    geom_doc.on('remove', this.geomDocRemove, this);
    geom_doc.on('beforeReplace', this.geomDocBeforeReplace, this);  
    geom_doc.on('replace', this.geomDocReplace, this);  

    SS.UI_EDITING_STATE.on('change', uiStateChanged, this);

    selectionManager.on('selected', this.selected, this);
    selectionManager.on('deselected', this.deselected, this);

    command_stack.on('beforePop', this.clear, this);

}
