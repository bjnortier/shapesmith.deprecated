var SS = SS || {};

SS.GeomNodeRenderingManager = function() {

    var hiddenByUser = [];

    this.geomDocAdd = function(geomNode) {
        if (SS.geomDoc.isRoot(geomNode)) {
            SS.renderGeometry(geomNode);
        }
        if (geomNode.isEditingOrTransformEditing()) {
            setOtherNonHiddenNodesTransparent(geomNode);
        }
    }

    this.geomDocBeforeRemove = function(geomNode) {
        SS.selectionManager.deselectID(geomNode.id);
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
        SS.selectionManager.deselectID(original.id);
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
        SS.geomDoc.rootNodes.map(function(geomNode) {
            if (hiddenByUser.indexOf(geomNode.id) === -1) {
                SS.restoreOpacity(geomNode);
            }
        });
    }

    var setOtherNonHiddenNodesTransparent = function(geomNode) {
        SS.geomDoc.rootNodes.map(function(rootNode) {
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
                SS.unhighlightGeometry(SS.geomDoc.findById(id))
            }
        }
    }

    this.selected = function(selected) {
        for (var i in selected) {
            var id = selected[i];
            var geomNode = SS.geomDoc.findById(id);
            if (hiddenByUser.indexOf(id) === -1) {
                SS.highlightGeometry(geomNode);
            }
            setOtherNonHiddenNodesTransparent(geomNode);
        }
    }

    this.clear = function() {
        SS.selectionManager.deselectAll();
    }

    this.setOpaque = function(id) {
        hiddenByUser.splice(hiddenByUser.indexOf(id), 1);
        SS.renderGeometry(SS.geomDoc.findById(id));
    }

    this.setHidden = function(id) {
        hiddenByUser.push(id);
        SS.hideGeometry(SS.geomDoc.findById(id));
    }
    
    SS.geomDoc.on('add', this.geomDocAdd, this);
    SS.geomDoc.on('beforeRemove', this.geomDocBeforeRemove, this);
    SS.geomDoc.on('remove', this.geomDocRemove, this);
    SS.geomDoc.on('beforeReplace', this.geomDocBeforeReplace, this);  
    SS.geomDoc.on('replace', this.geomDocReplace, this);  

    SS.UI_EDITING_STATE.on('change', uiStateChanged, this);

    SS.selectionManager.on('selected', this.selected, this);
    SS.selectionManager.on('deselected', this.deselected, this);

    SS.commandStack.on('beforePop', this.clear, this);

}
