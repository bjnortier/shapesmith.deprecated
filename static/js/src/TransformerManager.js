var SS = SS || {};

SS.TransformerManager = function() {

    var that = this;
    var UISTATE = {DEACTIVATED: 0, SCALE: 1, ROTATE: 2};
    var uiState = UISTATE.DEACTIVATED;
    var transformingState = undefined;
    var lastWorkplanePosition = undefined;
    
    var initiators = [];
    
    var activateScaleAndTranslate = function(geomNode) {
        initiators.map(function(initiator) {
            initiator.destroy();
        });
        initiators = [];
        initiators.push(new SS.TranslateTransformerInitiator({geomNode: geomNode}));
        initiators.push(new SS.ScaleTransformerInitiator({geomNode: geomNode}));
    }

    var activateRotateAndTranslate = function(geomNode) {
        initiators.map(function(initiator) {
            initiator.destroy();
        });
        initiators = [];
        initiators.push(new SS.RotateTransformerInitiator({geomNode: geomNode}));
        initiators.push(new SS.TranslateTransformerInitiator({geomNode: geomNode}));
    }
    
    var activate = function(geomNode) {
        if (uiState === UISTATE.SCALE) {
            activateRotateAndTranslate(geomNode);
            uiState = UISTATE.ROTATE;
        } else {
            activateScaleAndTranslate(geomNode);
            uiState = UISTATE.SCALE;
        }
    }

    this.selected = function(selected) {
        if (selectionManager.size() === 1) {
            node = geom_doc.findById(selectionManager.getSelected()[0]);
            if (!node.isEditingOrTransformEditing()) {
                activate(node);
            }
        }
    }

    this.deselected = function(deselected) {
        uiState = UISTATE.UNDEFINED;
    }

    this.clear = function() {
        initiators.map(function(initiator) {
            initiator.destroy();
        });
        uiState = UISTATE.UNDEFINED;
    }

    selectionManager.on('selected', this.selected, this);
    selectionManager.on('deselected', this.deselected, this);  

    command_stack.on('beforePop', this.clear, this);
}
