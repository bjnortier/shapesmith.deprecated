var SS = SS || {};

SS.GeomNodeModel = Backbone.Model.extend({

    initialize: function() { 
        this.geomNode = this.attributes.geomNode;
        this.selected = false;
        this.sceneView = new SS.GeomNodeSceneObjectView({model: this});

        SS.selectionManager.on('selected', this.select, this);
        SS.selectionManager.on('deselected', this.deselect, this);
        SS.editingState.on('change', this.editingStageChange, this);
    },

    destroy: function() {
        this.sceneView.remove();

        SS.selectionManager.off('selected', this.select, this);
        SS.selectionManager.off('deselected', this.deselect, this);
        SS.editingState.off('change', this.editingStageChange, this);
    },

    select: function(selected) {
        this.trigger('selected', selected);
    },

    deselect: function(deselected) {
        this.trigger('deselected', deselected);
    },

    editingStageChange: function() {
        this.trigger('editingStageChange');
    },


});

SS.GeomNodeSceneObjectView = SS.InteractiveSceneView.extend({

    initialize: function() {
        SS.InteractiveSceneView.prototype.initialize.call(this);
        this.render();
        this.highlightable = false;
        this.active = false;
        this.model.on('selected', this.select, this);
        this.model.on('deselected', this.deselect, this);
        this.model.on('editingStageChange', this.editingStageChange, this);
    },


    remove: function() {
        SS.InteractiveSceneView.prototype.remove.call(this);
        this.model.on('selected', this.select, this);
        this.model.on('deselected', this.deselect, this);
        this.model.on('editingStageChange', this.editingStageChange, this);
    },

    updateCameraScale: function() {
        // Prevent re-rendering on camera change;
        return false;
    },

    render: function() {
        this.clear();

        var geometries = SS.createGeometry(this.model.geomNode);
        if (geometries) {
            this.sceneObject.add(geometries.faces);
            this.sceneObject.add(geometries.edges);
            this.hiddenSceneObject.add(geometries.selectionForEdges);
            this.geometries = geometries;
        } 

        this.postRender();
        return this;
    },

    mouseUp: function(event) {
        if (SS.mouseState.isFree() && !SS.editingState.isEditing()) {
            if (event.ctrlKey || event.metaKey) {
                SS.selectionManager.shiftPick(this.model.geomNode.id);
            } else if (!event.shiftKey) {
                SS.selectionManager.pick(this.model.geomNode.id);
            }
        }
    },

    select: function(selected) {
        var thisNodeSelected = selected.indexOf(this.model.geomNode.id) !== -1;
        var anySelected = SS.selectionManager.size() > 0;

        if (thisNodeSelected) {
            this.updateColor(0xdddd00);
        } 

        if (anySelected) {
            this.updateOpacity(0.7);
        } else {
            this.updateOpacity(1.0);
        }
    },

    deselect: function(deselected) {
        var thisNodeDeselected = deselected.indexOf(this.model.geomNode.id) !== -1;
        var anySelected = SS.selectionManager.size() > 0;

        if (thisNodeDeselected) {
            this.updateColor(0x00dd00);
        } 

        if (anySelected) {
            this.updateOpacity(0.7);
        } else {
            this.updateOpacity(1.0);
        }
    },

    editingStageChange: function() {
        if (SS.editingState.editing) {
            this.updateOpacity(0.7);
        } else {
            this.updateOpacity(1.0);
        }
    },

    updateColor: function(color) {
        if (this.geometries) {
            this.geometries.faces.children.map(function(child) {
                child.material.color.setHex(color);
                child.material.ambient.setHex(color);
                child.material.specular.setHex(color);
            });
            this.geometries.edges.children.map(function(child) {
                child.material.color.setHex(color);
            });
        }
    },

    updateOpacity: function(opacity) {
        if (this.geometries) {
            this.geometries.faces.children.map(function(child) {
                child.material.opacity = opacity;
            });
            this.geometries.edges.children.map(function(child) {
                child.material.opacity = opacity;
            });
        }
    },

});