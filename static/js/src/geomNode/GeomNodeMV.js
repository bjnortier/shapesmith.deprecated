var SS = SS || {};

SS.GeomNodeModel = Backbone.Model.extend({

    initialize: function() { 
        this.geomNode = this.attributes.geomNode;
        this.node = this.geomNode;
        this.selected = false;
        this.hidden = false;
        this.geomNodeView = new SS.GeomNodeSceneObjectView({model: this}),
        this.transformViews = [];
        this.lastTransformViewState = undefined;

        SS.selectionManager.on('selected', this.select, this);
        SS.selectionManager.on('deselected', this.deselect, this);
        SS.editingState.on('change', this.editingStageChange, this);
    },

    destroy: function() {
        var that = this;
        this.geomNodeView.remove();
        this.transformViews.map(function(view) {
            view.remove();
        });

        SS.selectionManager.off('selected', this.select, this);
        SS.selectionManager.off('deselected', this.deselect, this);
        SS.editingState.off('change', this.editingStageChange, this);
    },

    updateTransformViews: function() {
        this.transformViews.map(function(view) {
                view.remove();
        });

        if ((SS.selectionManager.size() === 1) && 
            (SS.selectionManager.getSelected()[0] === this.geomNode.id)) {

            if ((this.lastTransformViewState === undefined) || 
                (this.lastTransformViewState === 'translaterotate')) {

                this.transformViews = [
                    new SS.TranslateTransformerView({model: this}),
                    new SS.ScaleBoxView({model: this}),
                    new SS.ScaleFootprintView({model: this}),
                ];

                // Separate reference as we need to know which
                // arrow initiated the scaling
                this.arrowViews = [
                new SS.ScaleArrowViewMaxXMaxY({model: this}),
                    new SS.ScaleArrowViewMaxXMinY({model: this}),
                    new SS.ScaleArrowViewMinXMinY({model: this}),
                    new SS.ScaleArrowViewMinXMaxY({model: this}),
                ];
                this.transformViews = this.transformViews.concat(this.arrowViews);

                this.lastTransformViewState = 'translatescale';

            } else if (this.lastTransformViewState === 'translatescale') {
                this.transformViews = [
                    new SS.TranslateTransformerView({model: this}),
                    new SS.WorkplaneURotationPreview({model: this, radius: this.normalizedBoundingRadius + 10, index: 0}),
                    new SS.WorkplaneVRotationPreview({model: this, radius: this.normalizedBoundingRadius + 10, index: 1}),
                    new SS.WorkplaneWRotationPreview({model: this, radius: this.normalizedBoundingRadius + 10, index: 2}),
                ];

                this.lastTransformViewState = 'translaterotate';
            }

        } 
    },

    select: function(selected) {
        this.trigger('selected', selected);
        this.updateTransformViews();
    },

    deselect: function(deselected) {
        this.trigger('deselected', deselected);
        this.updateTransformViews();
    },

    editingStageChange: function() {
        this.trigger('editingStageChange');
    },

    mouseDownOnTranslate: function(translateView) {
        
        var geomNode = this.geomNode;
        var editingNode = geomNode.editableCopy();
        var transform = new Transform({
            type: 'translate',
            editing: true,
            origin: {x: Math.round(this.normalizedCenter.x), 
                     y: Math.round(this.normalizedCenter.y), 
                     z: 0},
            parameters: {u: 0.0,
                         v: 0.0,
                         w: 0.0,
                         n: 0}
        });

        editingNode.transforms.push(transform);
        geomNode.originalSceneObjects = geomNode.sceneObjects;

        SS.selectionManager.deselectID(geomNode.id);
        SS.geomDoc.replace(geomNode, editingNode);

        // new SS.GeomNodeTranslateEditingModel({
        //     originalNode: geomNode,
        //     editingNode: editingNode, 
        //     transform: transform,
        //     initiatorModel: this,
        // });

        this.destroy();
    },


});

SS.GeomNodeSceneObjectView = SS.InteractiveSceneView.extend({

    initialize: function() {
        SS.InteractiveSceneView.prototype.initialize.call(this);

        this.geometries = SS.createGeometry(this.model.geomNode);
        this.model.geomNode.geometries = this.geometries;
        this.render();
        this.highlightable = false;
        this.active = false;
        this.priority = 0;
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

        if (this.geometries) {
            this.sceneObject.add(this.geometries.faces);
            this.sceneObject.add(this.geometries.edges);
            this.hiddenSceneObject.add(this.geometries.selectionForEdges);

            this.model.boundingBox = SS.boundingBoxForGeomNode(this.model.geomNode); 
            this.model.normalizedBoundingBox = SS.normalizedBoundingBoxForGeomNode(this.model.geomNode);
            this.model.normalizedCenter = SS.centerOfGeom(this.model.normalizedBoundingBox);
            this.model.normalizedBoundingRadius = SS.normalizedBoundingRadius(this.model.normalizedBoundingBox);
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

SS.GeomNodeEditingModel = SS.NodeModel.extend({

    initialize: function() { 
        this.originalNode = this.attributes.originalNode;
        this.editingNode = this.attributes.editingNode;
        this.node = this.attributes.transform;

        // Initial node dimensions from the initiator node
        var initiatorModel = this.attributes.initiatorModel;
        this.boundingBox = initiatorModel.boundingBox;
        this.normalizedBoundingBox = initiatorModel.normalizedBoundingBox;
        this.normalizedCenter =  initiatorModel.normalizedCenter;
        this.normalizedBoundingRadius =  initiatorModel.normalizedBoundingRadius;

        this.views = [
            new SS.NodeDOMView({model: this}),
            new SS.OkCancelView({model: this}),
        ];

        SS.geomDoc.on('replace', this.geomDocReplace, this);
        SS.commandStack.on('beforePop', this.cancel, this);
    },

    destroy: function() {
        this.views.map(function(view) {
            view.remove();
        });
        SS.geomDoc.off('replace', this.geomDocReplace);
        SS.commandStack.off('beforePop', this.cancel, this);
    },

    getBoundingBox: function() {
        return this.boundingBox;
    },

    tryCommit: function() {
        var cmd =  update_geom_command(this.originalNode, 
                                       this.editingNode, 
                                       this.editingNode); 
        SS.commandStack.execute(cmd);
    },

    cancel: function() {
        this.destroy();
        SS.geomDoc.replace(this.editingNode, this.originalNode);
    },

    geomDocReplace: function(original, replacement) {
        if (original === this.editingNode) {
            this.destroy();
        } 
        
    },


});

SS.GeomNodeTransformEditingModel = SS.GeomNodeEditingModel.extend({
    
    initialize: function() { 
        SS.GeomNodeEditingModel.prototype.initialize.call(this);
        this.node = this.attributes.transform;
    },

});

SS.GeomNodeTranslateEditingModel = SS.GeomNodeTransformEditingModel.extend({

    initialize: function() { 
        SS.GeomNodeTransformEditingModel.prototype.initialize.call(this);

        this.translateView = new SS.TranslateTransformerView({model: this});
        SS.sceneView.addToMouseOverAndMouseDown(this.translateView);
            
        this.views.concat = [
            this.translateView,
            new SS.TranslateHeightCursoid({model: this}),
            new SS.TranslatedGeomNodeSceneObjectView({model: this}),
            new SS.TranslateDimensionArrows({model: this}),
            new SS.TranslateDimensionText({model: this}),
        ];
    }

});

SS.GeomNodeEditingSceneObjectView = SS.InteractiveSceneView.extend({

    initialize: function() {
        SS.InteractiveSceneView.prototype.initialize.call(this);

        this.originalGeometries = this.model.originalNode.geometries;
        this.editingGeometries = this.originalGeometries;
        this.render();
        this.highlightable = false;
        this.active = false;
        this.priority = 0;
    },

    remove: function() {
        SS.InteractiveSceneView.prototype.remove.call(this);
    },

    render: function() {
        this.clear();

        // if (this.geometries) {
        //     this.sceneObject.add(this.geometries.faces);
        //     this.sceneObject.add(this.geometries.edges);

        //     this.model.normalizedBoundingBox = SS.normalizedBoundingBoxForGeomNode(this.model.editingNode);
        //     this.model.normalizedCenter = SS.centerOfGeom(this.model.normalizedBoundingBox);
        //     this.model.normalizedBoundingRadius = SS.normalizedBoundingRadius(this.model.normalizedBoundingBox);
        // } 

        this.postRender();
        return this;
    },
});

SS.TranslatedGeomNodeSceneObjectView = SS.GeomNodeEditingSceneObjectView.extend({

    initialize: function() {
        SS.GeomNodeEditingSceneObjectView.prototype.initialize.call(this);
        this.model.on('beforeChange', this.update, this);
    },

    remove: function() {
        SS.GeomNodeEditingSceneObjectView.prototype.remove.call(this);
        this.model.off('beforeChange', this.update, this);
    },

    update: function() {
        var transform = this.model.node;
        var position = SS.objToVector(transform.parameters);
        this.editingGeometries = SS.translateGeomNodeRendering(this.originalGeometries, this.editingGeometries, position);
        this.render();
    }

});


