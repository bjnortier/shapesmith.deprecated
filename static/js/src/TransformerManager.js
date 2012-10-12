var SS = SS || {};

SS.TransformerManager = function() {

    var that = this;
    var STATES = {DEACTIVATED: 0, SCALE: 1, ROTATE: 2};
    var state = STATES.DEACTIVATED;
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
        initiators.push(new SS.TranslateTransformerInitiator({geomNode: geomNode}));
        initiators.push(new SS.RotateTransformerInitiator({geomNode: geomNode}));
    }
    
    var activate = function(geomNode) {
        if (state === STATES.SCALE) {
            activateRotateAndTranslate(geomNode);
            state = STATES.ROTATE;
        } else {
            activateScaleAndTranslate(geomNode);
            state = STATES.SCALE;
        }
    }

    this.selected = function(selected) {
        if (SS.selectionManager.size() === 1) {
            node = SS.geomDoc.findById(SS.selectionManager.getSelected()[0]);
            if (!node.isEditingOrTransformEditing() 
                &&
                !SS.geomNodeRenderingManager.isHiddenByUser(node)) {
                activate(node);
            }
        }
    }

    this.deselected = function(deselected) {
        state = STATES.UNDEFINED;
    }

    this.clear = function() {
        initiators.map(function(initiator) {
            initiator.destroy();
        });
        state = STATES.UNDEFINED;
    }

    SS.selectionManager.on('selected', this.selected, this);
    SS.selectionManager.on('deselected', this.deselected, this);  

    SS.commandStack.on('beforePop', this.clear, this);
}
