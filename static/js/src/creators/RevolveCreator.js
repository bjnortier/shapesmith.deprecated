var SS = SS || {};

SS.RevolveCreator = SS.ParentCreator.extend({
    
    initialize: function(attributes) { 
        attributes.noOriginCorner = true;
        SS.ParentCreator.prototype.initialize.call(this, attributes);
        
        if (this.originalNode) {
            this.boundingBox = SS.boundingBoxForGeomNode(this.editingNode);
        } else {
            this.node.parameters.u = 0;
            this.node.parameters.v = 0;
            this.node.parameters.w = 1;
            this.node.parameters.angle = 360;

            this.views.push(new SS.ParentGeomNodeView({model: this}));
            this.trigger('change:model', this);
        }

        this.views = this.views.concat([
            new SS.DraggableOriginCorner({model: this}),
            new SS.RevolveAxisPreview({model: this}),
            new SS.XYZAxisChoice({model: this}),
        ]);
        this.trigger('change', this);
    },
    
    getBoundingBox: function() {
        return this.boundingBox;
    },
    
});


SS.ParentGeomNodeView = SS.SceneObjectView.extend({

    initialize: function() {
        SS.SceneObjectView.prototype.initialize.call(this);
        this.model.on('change:model', this.render, this);
        this.meshes = SS.createGeometry(this.model.childNode);
        this.changeToPreviewColor(this.meshes);
        this.render();
    },

    remove: function() {
        SS.SceneObjectView.prototype.remove.call(this);
        this.model.off('change:model', this.render);
    },
    
    dontApplyWorkplane: true,

    render: function() {
        this.clear();
        this.sceneObject.add(this.meshes['faces']);
        this.sceneObject.add(this.meshes['edges']);
        this.postRender();
    },
    
    changeToPreviewColor: function(meshes) {
        meshes.faces.children.map(function(child) {
            child.material = SS.materials.faceMaterial;
        });
        meshes.edges.children.map(function(child) {
            child.material = SS.materials.lineMaterial;

        });
    },

});


SS.RevolveAxisPreview = SS.PreviewWithOrigin.extend({

    initialize: function() {
        SS.PreviewWithOrigin.prototype.initialize.call(this);
        this.render();
    },

    render: function() {
        this.clear();
        SS.PreviewWithOrigin.prototype.render.call(this);

        var origin = this.model.node.origin;
        var u = this.model.node.parameters.u;
        var v = this.model.node.parameters.v;
        var w = this.model.node.parameters.w;
        var axisVector = new THREE.Vector3(u,v,w).normalize();
        
        var axis = new THREE.Geometry();
        axis.vertices.push(axisVector.clone().multiplyScalar(1000));
        axis.vertices.push(axisVector.clone().multiplyScalar(-1000));
        var line = new THREE.Line(axis, SS.materials.lineMaterial);  
        this.sceneObject.add(line);

        this.postRender();
    },

});