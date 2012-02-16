var SS = SS || {};

SS.WedgeCreator = SS.Creator.extend({

    initialize: function(attributes) {
        SS.Creator.prototype.initialize.call(this, attributes);

        this.node.parameters.u1 = 10;
        this.node.parameters.u2 = 5;
        this.node.parameters.v = 10;
        this.node.parameters.w = 10;

        this.views = this.views.concat([
            new SS.WedgePreview({model: this}),
            new SS.DraggableUVCorner({model: this, uKey: 'u1'}),
            new SS.DraggableU2Corner({model: this}),
        ]);
        this.trigger('change', this);
    },

    mouseDownOnUV: function(corner) {
        this.activateCorner(corner, SS.UVWHeightCursoid, {model: this, uKey: 'u1'});
    },

    mouseDownOnU2: function(corner) {
        this.activateCorner(corner);
    },

    getBoundingBox: function() {
        var origin = this.node.origin;
        var u1 = this.node.parameters.u;
        var v = this.node.parameters.v;
        var w = this.node.parameters.w;
        return {min: new THREE.Vector3(origin.x, origin.y, origin.z),
                max: new THREE.Vector3(origin.x + u1, origin.y + v, origin.z + w)};
    },


});

SS.WedgePreview = SS.PreviewWithOrigin.extend({

    initialize: function() {
        SS.PreviewWithOrigin.prototype.initialize.call(this);
        this.render();
    },
    
    render: function() {
        this.clear();
        SS.PreviewWithOrigin.prototype.render.call(this);
        
        var origin = this.model.node.origin;
        var u1 = this.model.node.parameters.u1;
        var u2 = this.model.node.parameters.u2;
        var v = this.model.node.parameters.v;
        var w = this.model.node.parameters.w;

        var materials = [ SS.constructors.faceMaterial, SS.constructors.wireframeMaterial ];

        if (u1 && v && w) {
	    var geometry = new THREE.WedgeGeometry(u1,v,w,u2 - u1);
	    var materials = [ SS.constructors.faceMaterial, SS.constructors.wireframeMaterial ];
	    var wedge =  THREE.SceneUtils.createMultiMaterialObject(geometry, materials);
	    wedge.position.x = origin.x + u1/2;
	    wedge.position.y = origin.y + v/2;
	    wedge.position.z = origin.z +  w/2;
            this.sceneObject.add(wedge);

        } else if (u1 && v && u2) {

	    var uvLineGeom = new THREE.Geometry();
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u1, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u2, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    var uvLine = new THREE.Line(uvLineGeom, SS.constructors.lineMaterial);
            uvLine.position = new THREE.Vector3(origin.x, origin.y, origin.z);
	    this.sceneObject.add(uvLine);
        }


        this.postRender();
    },

});

SS.DraggableU2Corner = SS.DraggableCorner.extend({

    initialize: function(options) {
        SS.DraggableCorner.prototype.initialize.call(this, options);
        this.render();
    },

    priority: 2,

    mouseDown: function() {
        this.model.mouseDownOnU2(this);
    },

    cornerPositionFromModel: function() {
        return {x: this.model.node.origin.x + this.model.node.parameters.u2,
                y: this.model.node.origin.y + this.model.node.parameters.v,
                z: 0};
    },

    updateModelFromCorner: function(position) {
        var u2 = position.x - this.model.node.origin.x;
        this.model.node.parameters.u2 = Math.round(u2);
    },

});