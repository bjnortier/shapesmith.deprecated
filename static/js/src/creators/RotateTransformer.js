var SS = SS || {};

SS.RotateTransformerInitiator = SS.TransformerInitiator.extend({

    initialize: function(attributes) { 
        SS.TransformerInitiator.prototype.initialize.call(this, attributes);
        this.node = {origin: this.normalizedCenter, axis:{x:0, y:0, z:1}, angle:0};
        this.views = this.views.concat([
            new SS.WorkplaneURotationPreview({model: this, radius: this.normalizedBoundingRadius + 10, index: 0}),
            new SS.WorkplaneVRotationPreview({model: this, radius: this.normalizedBoundingRadius + 10, index: 1}),
            new SS.WorkplaneWRotationPreview({model: this, radius: this.normalizedBoundingRadius + 10, index: 2}),
        ]);
    },

    mouseDownOnArrow: function(axis, index) {
        
        var geomNode = this.originalNode;
        var editingNode = geomNode.editableCopy();
        var parameters = {u: axis.x,
                          v: axis.y,
                          w: axis.z,
                          angle: 0,
                          n: 0};
                
        var transform = new Transform({
            type: 'rotate',
            editing: true,
            origin: {x: Math.round(this.normalizedCenter.x), 
                     y: Math.round(this.normalizedCenter.y), 
                     z: Math.round(this.normalizedCenter.z)},
            parameters: parameters
        });
        editingNode.transforms.push(transform);
        geomNode.originalSceneObjects = geomNode.sceneObjects;

        SS.selectionManager.deselectID(geomNode.id);
        SS.geomDoc.replace(geomNode, editingNode);

        new SS.RotateTransformer({originalNode: geomNode,
                                  editingNode: editingNode, 
                                  transform: transform,
                                  mouseDownArrowViewIndex: index});
        this.destroy();
    },

});


SS.RotateTransformer = SS.Transformer.extend({

    initialize: function(attributes) { 
        SS.Transformer.prototype.initialize.call(this, attributes);

        if (!attributes.editingExisting) {
            this.anchorPosition = attributes.anchorPosition;
            var arrowViews = [
                new SS.WorkplaneURotationPreview({model: this, radius: this.normalizedBoundingRadius + 10}),
                new SS.WorkplaneVRotationPreview({model: this, radius: this.normalizedBoundingRadius + 10}),
                new SS.WorkplaneWRotationPreview({model: this, radius: this.normalizedBoundingRadius + 10}),
            ];
            SS.sceneView.addToMouseOverAndMouseDown(arrowViews[attributes.mouseDownArrowViewIndex]);
            arrowViews[attributes.mouseDownArrowViewIndex].mouseDown();
            var newViews = [
                new SS.RotateGeomNodeView({model: this}),
            ];
            this.views = this.views.concat(newViews);
            this.views = this.views.concat(arrowViews);
        }
    },

});

SS.RotateGeomNodeView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('beforeChange', this.render, this);
    },

    render: function() {
        var transform = this.model.node;
        SS.rotateGeomNodeRendering(this.model.originalNode, 
                                   this.model.editingNode, 
                                   transform);
    },

});

