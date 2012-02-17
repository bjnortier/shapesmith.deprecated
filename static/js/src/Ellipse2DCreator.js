var SS = SS || {};

SS.Ellipse2DCreator = SS.Creator.extend({

    initialize: function(attributes) {
        SS.Creator.prototype.initialize.call(this, attributes);

        this.node.parameters.r1 = 20;
        this.node.parameters.r2 = 10;

        this.views = this.views.concat([
            new SS.Ellipse2DPreview({model: this}),
            new SS.DraggableUVCorner({model: this, uKey: 'r1', vKey: 'r2'}),
        ]);
        this.trigger('change', this);
    },

    mouseDownOnUV: function(corner) {
        this.activateCorner(corner);
    },

    getBoundingBox: function() {
        var origin = this.node.origin;
        var r1 = this.node.parameters.r1;
        var r2 = this.node.parameters.r2;
        return {min: new THREE.Vector3(origin.x - r1, origin.y - r2, origin.z),
                max: new THREE.Vector3(origin.x + r1, origin.y + r2, origin.z)};
    },


});

SS.Ellipse2DPreview = SS.PreviewWithOrigin.extend({

    initialize: function() {
        SS.PreviewWithOrigin.prototype.initialize.call(this);
        this.render();
    },
    
    render: function() {
        this.clear();
        SS.PreviewWithOrigin.prototype.render.call(this);

        var origin = this.model.node.origin;
        var r1 = this.model.node.parameters.r1;
        var r2 = this.model.node.parameters.r2;
        var h = this.model.node.parameters.h;

        if (r1 && r2) {

	    var ellipseGeom = new THREE.EllipseGeometry(r1, r2);
	    var ellipseFace = new THREE.Mesh(ellipseGeom, SS.constructors.faceMaterial);
            ellipseFace.doubleSided = true;
            this.sceneObject.add(ellipseFace);

            var ellipseWireGeom = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
		var theta = Math.PI*2*i/50;
		var dx = r1*Math.cos(theta);
		var dy = r2*Math.sin(theta);
		ellipseWireGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	    }
	    var ellipseWire = new THREE.Line(ellipseWireGeom, SS.constructors.lineMaterial);
            this.sceneObject.add(ellipseWire);

            if (origin.z !== 0) {
	        var ellipseBaseWire = new THREE.Line(ellipseWireGeom, SS.constructors.lineMaterial);
                ellipseBaseWire.position = new THREE.Vector3(0, 0, -origin.z);
                this.sceneObject.add(ellipseBaseWire);
            }
        }
       
        this.postRender();
    },

});


