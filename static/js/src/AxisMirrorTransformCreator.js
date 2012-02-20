var SS = SS || {};

SS.AxisMirrorTransformCreator = SS.TransformCreator.extend({

    initialize: function(attributes) {
        SS.TransformCreator.prototype.initialize.call(this, attributes);
        this.node.parameters = {u: 0, v: 0, w: 1, n:0};

        this.views = this.views.concat([
            new SS.AxisMirrorTransformPreview({model: this}),
        ]);
        this.trigger('change', this);
    },

    getBoundingBox: function() {
        if (!this.attributes.editingExisting) {
            return SS.boundingBoxForGeomNode(this.editingNode);
        } else {
            return undefined;
        }
    },

    mouseDownOnOrigin: function(corner) {
        this.activateCorner(corner);
    },

});

SS.AxisMirrorTransformPreview = SS.PreviewWithOrigin.extend({

    initialize: function() {
        SS.PreviewWithOrigin.prototype.initialize.call(this);
        this.render();
    },

    render: function() {
        if (this.model.originalNode.originalSceneObjects) {
            var transform = this.model.node;
            var position = new THREE.Vector3(transform.parameters.u,
                                             transform.parameters.v,
                                             transform.parameters.w);
            

            SS.axisMirrorGeomNodeRendering(this.model.originalNode, 
                                           this.model.editingNode, 
                                           this.model.node);
        }

        this.clear();
        SS.PreviewWithOrigin.prototype.render.call(this);

        var origin = this.model.node.origin;
        var u = this.model.node.parameters.u;
        var v = this.model.node.parameters.v;
        var w = this.model.node.parameters.w;
        var axisVector = new THREE.Vector3(u,v,w).normalize();
        
        var axis = new THREE.Geometry();
        axis.vertices.push(new THREE.Vertex(axisVector.clone().multiplyScalar(1000)));
        axis.vertices.push(new THREE.Vertex(axisVector.clone().multiplyScalar(-1000)));
        var line = new THREE.Line(axis, SS.constructors.lineMaterial);  
        this.sceneObject.add(line);

        this.postRender();
    },

});
