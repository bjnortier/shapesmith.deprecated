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

    _.extend(this, Backbone.Events);

    var editing = false;

    this.__defineGetter__("editing", function() {
        return editing;
    });

    this.__defineSetter__("editing", function(val) {
        if (editing !== val) {
            editing = val;
            this.trigger('change', editing);
        }
    });

    this.isEditing = function() {
        return this.editing;
    }

    this.geomDocAdd = function(node) {
        if (node.isEditingOrTransformEditing() && !this.editing) {
            editing = true;
            this.trigger('change', editing);
        }
    }

    this.geomDocRemove = function(node) {
        if (node.isEditingOrTransformEditing() && this.editing) {
            editing = false;
            this.trigger('change', editing);
        }
    }

    this.geomDocReplace = function(original, replacement) {
        if (editing !== replacement.isEditingOrTransformEditing()) {
            editing = replacement.isEditingOrTransformEditing();
            this.trigger('change', editing);
        }
    }

    geom_doc.on('add', this.geomDocAdd, this);
    geom_doc.on('remove', this.geomDocRemove, this);
    geom_doc.on('replace', this.geomDocReplace, this);
    
}

var geom_doc = new GeomDocument();
SS.UI_MOUSE_STATE = new SS.UIMouseState();
SS.UI_EDITING_STATE = new SS.UIEditingState();
