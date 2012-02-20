var SS = SS || {};

SS.PrismCreator = SS.ParentCreator.extend({
    
    initialize: function(attributes) { 
        attributes.noOriginCorner = true;
        SS.ParentCreator.prototype.initialize.call(this, attributes);
        
        if (!this.originalNode) {
            this.node.parameters.u = 0;
            this.node.parameters.v = 0;
            this.node.parameters.w = 10;
        }

        // Editing - use the existing object
        if (this.originalNode) {
            this.boundingBox = SS.boundingBoxForGeomNode(this.editingNode);
            this.center = SS.centerOfGeom(this.boundingBox);
        } else {
            this.views.push(new SS.PrismGeomNodeView({model: this}));
            this.trigger('change:model', this);
        }

        this.views.push(new SS.PrismUVCorner({model: this}));
        this.views.push(new SS.PrismHeightCursoid({model: this}));
        this.trigger('change', this);
    },

    
    
    mouseDownOnUV: function(corner) {
    },

    getBoundingBox: function() {
        return this.boundingBox;
    },
    
});

SS.PrismGeomNodeView = SS.SceneObjectView.extend({

    initialize: function() {
        SS.SceneObjectView.prototype.initialize.call(this);
        this.model.on('change:model', this.render, this);
        this.bottomMeshes = SS.createGeometry(this.model.childNode);
        this.changeToPreviewColor(this.bottomMeshes);
        this.render();
    },

    remove: function() {
        SS.SceneObjectView.prototype.remove.call(this);
        this.model.off('change:model', this.render);
    },

    render: function() {
        this.clear();
        this.sceneObject.add(this.bottomMeshes['faces']);
        this.sceneObject.add(this.bottomMeshes['edges']);

        var translation = new THREE.Vector3(this.model.node.parameters.u,
                                            this.model.node.parameters.v,
                                            this.model.node.parameters.w);
        var topMeshes = SS.createGeometry(this.model.childNode);
        ['faces', 'edges'].map(function(topology) {
            var meshes = topMeshes[topology];
            for (var i = 0; i < meshes.children.length; ++i) {
                var geometry = meshes.children[i].geometry;
                geometry.vertices = geometry.vertices.map(function(vertex) {
	            var position = vertex.position.clone();
	            position.x = position.x + translation.x;
	            position.y = position.y + translation.y;
	            position.z = position.z + translation.z;
	            return new THREE.Vertex(position);
                });
	    }
        });

        this.changeToPreviewColor(topMeshes);

        this.model.boundingBox = SS.boundingBoxForSceneObject(this.sceneObject);
        this.model.center = SS.centerOfGeom(this.model.boundingBox);
        
        this.sceneObject.add(topMeshes['faces']);
        this.sceneObject.add(topMeshes['edges']);

        var origin = this.model.node.origin;
        this.sceneObject.position = new THREE.Vector3(origin.x, origin.y, origin.z);

        this.postRender();
    },
    
    changeToPreviewColor: function(meshes) {
        meshes.faces.children.map(function(child) {
            child.material = SS.constructors.faceMaterial;
        });
        meshes.edges.children.map(function(child) {
            child.material = SS.constructors.lineMaterial;

        });
    },

});

SS.PrismUVCorner = SS.DraggableCorner.extend({

    initialize: function(options) {
        SS.DraggableCorner.prototype.initialize.call(this, options);
        this.render();
    },

    priority: 1,

    mouseDown: function() {
        this.model.mouseDownOnUV(this);
    },

    cornerPositionFromModel: function() {
        return {x: this.model.center.x + this.model.node.parameters.u,
                y: this.model.center.y + this.model.node.parameters.v,
                z: 0};
    },

    updateModelFromCorner: function(position) {
        var u = position.x - this.model.center.x;
        var v = position.y - this.model.center.y;

        this.model.node.parameters.u = Math.round(u);
        this.model.node.parameters.v = Math.round(v);
    },

});

SS.PrismHeightCursoid = SS.HeightCursoid.extend({

     initialize: function(options) {
	 SS.HeightCursoid.prototype.initialize.call(this);
         this.render();
    },

    cornerPositionFromModel: function() {
        var parameters = this.model.node.parameters
        return {x: this.model.center.x + this.model.node.parameters.u,
                y: this.model.center.y + this.model.node.parameters.v,
                z: this.model.node.parameters.w};
    },    

    updateModelFromCorner: function(position) {
        var parameters = this.model.node.parameters
        var center = this.model.center;
        parameters.w = position.z;
    },

});