var SS = SS || {};

SS.CylinderCreator = SS.Creator.extend({

    initialize: function(attributes) {
        SS.Creator.prototype.initialize.call(this, attributes);

        this.node.parameters.r = 10;
        this.node.parameters.h = 10;
        this.node.extra = {angle: 0};

        this.views = this.views.concat([
            new SS.CylinderPreview({model: this}),
            new SS.DraggableRadiusCorner({model: this}),
        ]);
        this.trigger('change', this);
    },

    mouseDownOnRadius: function(corner) {
        if (this.activeCorner === corner) {
            return;
        }
        this.activeCornerView && this.activeCornerView.remove();
        this.activeCorner = corner;
        this.activeCornerView = new SS.RadiusHeightCursoid({model: this});
    },

    getBoundingBox: function() {
        var origin = this.node.origin;
        var r = this.node.parameters.r;
        var h = this.node.parameters.h;
        return {min: new THREE.Vector3(origin.x - r, origin.y - r, origin.z),
                max: new THREE.Vector3(origin.x + r, origin.y + r, origin.z + h)};
    },


});

SS.CylinderPreview = SS.PreviewWithOrigin.extend({

    initialize: function() {
        SS.PreviewWithOrigin.prototype.initialize.call(this);
        this.render();
    },
    
    render: function() {
        this.clear();
        SS.PreviewWithOrigin.prototype.render.call(this);

        var origin = this.model.node.origin;
        var r = this.model.node.parameters.r;
        var h = this.model.node.parameters.h;

        if (r) {
            var circleGeom1 = new THREE.Geometry(), circleGeom2 = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
		var theta = Math.PI*2*i/50;
		var dx = r*Math.cos(theta);
		var dy = r*Math.sin(theta);
		circleGeom1.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
		circleGeom2.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	    }
	    var circle1 = new THREE.Line(circleGeom1, SS.constructors.lineMaterial);
            circle1.position = new THREE.Vector3(origin.x, origin.y, origin.z);
            this.sceneObject.add(circle1);
        }

        if (r && h) {
	    var geometry = new THREE.CylinderGeometry(r, r, h, 20);
	    var cylinder = new THREE.Mesh(geometry, SS.constructors.faceMaterial);
	    cylinder.position = new THREE.Vector3(origin.x, origin.y, origin.z + h/2);
            cylinder.rotation.x = -Math.PI/2;

            var circle2 = new THREE.Line(circleGeom2, SS.constructors.lineMaterial);
	    circle2.position = new THREE.Vector3(origin.x, origin.y, origin.z + h);

            this.sceneObject.add(cylinder);
	    this.sceneObject.add(circle2);
        }
       
        this.postRender();
    },

});

SS.RadiusHeightCursoid = SS.HeightCursoid.extend({

     initialize: function(options) {
	 SS.HeightCursoid.prototype.initialize.call(this);
         this.render();
    },

    priority: 1,

    cornerPositionFromModel: function() {
        var r = this.model.node.parameters.r;
        var h = this.model.node.parameters.h;
        var angle = this.model.node.extra.angle;
        var dx = Math.cos(angle)*r;
        var dy = Math.sin(angle)*r;
        return {x: this.model.node.origin.x + dx,
                y: this.model.node.origin.y + dy,
                z: this.model.node.origin.z + h};
    },    

    updateModelFromCorner: function(position) {
        var dx = position.x - this.model.node.origin.x;
        var dy = position.y - this.model.node.origin.y;
        var angle = Math.atan2(dy, dx);

        var r = Math.sqrt(dx*dx + dy*dy);
        var h = position.z - this.model.node.origin.z;

        this.model.node.parameters.r = Math.round(r);
        this.model.node.parameters.h = Math.round(h);
        this.model.node.extra = {angle: angle};
    },

});

