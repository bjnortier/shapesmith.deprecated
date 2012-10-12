var SS = SS || {};

SS.GeomNodeModel = Backbone.Model.extend({

    initialize: function() { 
        this.geomNode = this.attributes.geomNode;
        this.selected = false;
        this.sceneView = new SS.GeomNodeSceneObjectView({model: this});
    },

    destroy: function() {
        this.sceneView.remove();
    },

    select: function() {
        this.selected = true;
        this.trigger('selectionChanged');
    },

    deselect: function() {
        this.selected = false;
        this.trigger('selectionChanged');
    }


});

SS.GeomNodeSceneObjectView = SS.InteractiveSceneView.extend({

    initialize: function() {
        SS.InteractiveSceneView.prototype.initialize.call(this);
        this.render();
        this.highlightable = false;
        this.model.on('selectionChanged', this.selectionChanged, this);
    },


    remove: function() {
        SS.InteractiveSceneView.prototype.remove.call(this);
        this.model.on('selectionChanged', this.selectionChanged, this);
    },

    render: function() {
        this.clear();

        var geometries = SS.createGeometry(this.model.geomNode);
        if (geometries) {
            this.sceneObject.add(geometries.faces);
            this.sceneObject.add(geometries.edges);

            // SS.sceneView.selectionOnlyMeshes.push(geometries.selectionForEdges);
            this.geometries = geometries;
        }

        this.postRender();
        return this;
    },

    mouseDown: function(event) {
    },

    mouseUp: function(event) {
        if (SS.UI_MOUSE_STATE.isFree() && !SS.UI_EDITING_STATE.isEditing()) {
            if (event.ctrlKey || event.metaKey) {
                SS.selectionManager.shiftPick(this.model.geomNode.id);
            } else if (!event.shiftKey) {
                SS.selectionManager.pick(this.model.geomNode.id);
            }
        }
    },

    selectionChanged: function() {
        if (this.geometries) {
            var color, opacity;
            if (this.model.selected) {
                color = 0xdddd00;
                opacity = 0.7;
            } else {
                color = 0x00dd00;
                opacity = 1.0;
            }
            this.geometries.faces.children.map(function(child) {
                child.material.color.setHex(color);
                child.material.ambient.setHex(color);
                child.material.specular.setHex(color);
                child.material.opacity = opacity;
            });
            this.geometries.edges.children.map(function(child) {
                child.material.color.setHex(color);
                child.material.opacity = opacity;
            });
        }
    },

});