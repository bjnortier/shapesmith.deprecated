var SS = SS || {};

SS.CuboidCreator = SS.Creator.extend({

    initialize: function(attributes) {
        SS.Creator.prototype.initialize.call(this, attributes);

        this.node.parameters.u = 10;
        this.node.parameters.v = 10;
        this.node.parameters.w = 10;

        this.views = this.views.concat([
            new SS.CuboidPreview({model: this}),
            new SS.DraggableUVCorner({model: this}),
        ]);
        this.trigger('change', this);
    },

    mouseDownOnUVW: function(corner) {
        this.activateCorner(corner, SS.UVWHeightCursoid);
    },

});

SS.CuboidPreview = SS.PreviewWithOrigin.extend({

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

        var materials = [ SS.constructors.faceMaterial, SS.constructors.wireframeMaterial ];

        if(u && v && w) {
            var geometry = new THREE.CubeGeometry(u,v,w);
	    var cube = THREE.SceneUtils.createMultiMaterialObject(geometry, materials);
	    cube.position.x = origin.x + u/2;
	    cube.position.y = origin.y + v/2;
	    cube.position.z = origin.z + w/2;
	    this.sceneObject.add(cube);
        } else if (u && v) {
            var planeGeom = new THREE.PlaneGeometry(u, v);
            var plane = THREE.SceneUtils.createMultiMaterialObject(planeGeom, materials);
            plane.position = new THREE.Vector3(origin.x + u/2, origin.y + v/2, origin.z);
            
	    this.sceneObject.add(plane);
        }


        this.postRender();
    },

});


SS.DraggableUVCorner = SS.DraggableCorner.extend({

    initialize: function(options) {
        SS.DraggableCorner.prototype.initialize.call(this, options);
        this.render();
    },

    priority: 1,

    mouseDown: function() {
        this.model.mouseDownOnUVW && this.model.mouseDownOnUVW(this);
    },

    cornerPositionFromModel: function() {
        return {x: this.model.node.origin.x + this.model.node.parameters.u,
                y: this.model.node.origin.y + this.model.node.parameters.v,
                z: 0};
    },

    updateModelFromCorner: function(position) {
        var u = position.x - this.model.node.origin.x;
        var v = position.y - this.model.node.origin.y;
        var w = position.z - this.model.node.origin.z;

        this.model.node.parameters.u = Math.round(u);
        this.model.node.parameters.v = Math.round(v);
    },

});

SS.UVWHeightCursoid = SS.HeightCursoid.extend({

     initialize: function(options) {
	 SS.HeightCursoid.prototype.initialize.call(this);
         this.render();
    },

    cornerPositionFromModel: function() {
        var parameters = this.model.node.parameters
        return {x: this.model.node.origin.x + this.model.node.parameters.u,
                y: this.model.node.origin.y + this.model.node.parameters.v,
                z: this.model.node.origin.z + this.model.node.parameters.w};
    },    

    updateModelFromCorner: function(position) {
        var parameters = this.model.node.parameters
        var origin = this.model.node.origin;
        parameters.u = position.x - origin.x;
        parameters.v = position.y - origin.y;
        parameters.w = position.z - origin.z;
    },

});
