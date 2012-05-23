var SS = SS || {};

SS.UIMouseState = function() {

    this.rotating = false;
    this.panning = false;
    this.popupShowing = false;

    this.isFree = function() {
        return (!(this.rotating || this.panning || this.popupShowing));
    }

    this.free = function() {
        this.rotating = false;
        this.panning = false;
    }

}

SS.UIEditingState = function() {

    this.editing = false;

    this.isEditing = function() {
        return this.editing;
    }

    this.geomDocAdd = function(node) {
        if (node.isEditingOrTransformEditing()) {
            this.editing = true;
        }
    }

    this.geomDocRemove = function(node) {
        if (node.isEditingOrTransformEditing()) {
            this.editing = false;
        }
    }

    this.geomDocReplace = function(original, replacement) {
        this.editing = replacement.isEditingOrTransformEditing();
    }

    geom_doc.on('add', this.geomDocAdd, this);
    geom_doc.on('remove', this.geomDocRemove, this);
    geom_doc.on('replace', this.geomDocReplace, this);
    
}

var geom_doc = new GeomDocument();
SS.UI_MOUSE_STATE = new SS.UIMouseState();
SS.UI_EDITING_STATE = new SS.UIEditingState();
