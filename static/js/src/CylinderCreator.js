var SS = SS || {};

SS.CylinderCreator = SS.PrimitiveCreator.extend({

    initialize: function(attributes) {
        SS.PrimitiveCreator.prototype.initialize.call(this, attributes);

        this.node.extra = {angle: 0};

        this.views = this.views.concat([
            new SS.CylinderPreview({model: this}),
            new SS.DraggableRadiusCorner({model: this}),
            new SS.CylinderDimensionArrows({model: this}),
            new SS.CylinderDimensionText({model: this}),
        ]);
        this.trigger('change', this);
    },
    
    setDefaultParamters: function() {
        this.node.parameters.r = 10;
        this.node.parameters.h = 10;
    },

    mouseDownOnRadius: function(corner) {
        this.activateCorner(corner, SS.RadiusHeightCursoid);
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
            this.sceneObject.add(circle1);

            if (origin.z) {
                var baseWire = new THREE.Line(circleGeom1, SS.constructors.lineMaterial);
                baseWire.position.z = -origin.z;
                this.sceneObject.add(baseWire);
            }
        }


        if (r && h) {
	    var geometry = new THREE.CylinderGeometry(r, r, h, 20);
	    var cylinder = new THREE.Mesh(geometry, SS.constructors.faceMaterial);
	    cylinder.position.z = h/2;
            cylinder.rotation.x = -Math.PI/2;

            var circle3 = new THREE.Line(circleGeom2, SS.constructors.lineMaterial);
	    circle3.position.z = h;

            this.sceneObject.add(cylinder);
	    this.sceneObject.add(circle3);
        }
       
        this.postRender();
    },

});

SS.RadiusHeightCursoid = SS.HeightCursoid.extend({

     initialize: function(options) {
	 SS.HeightCursoid.prototype.initialize.call(this);
         this.key = options.key || 'r';
         this.render();
    },

    priority: 1,

    cornerPositionFromModel: function() {
        var r = this.model.node.parameters[this.key];
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

        this.model.node.parameters[this.key] = Math.round(r);
        this.model.node.parameters.h = Math.round(h);
        this.model.node.extra = {angle: angle};
    },

});


SS.CylinderDimensionArrows = SS.SceneObjectView.extend({

    render: function() {
        this.clear();

        var origin = this.model.node.origin;
        var r = this.model.node.parameters.r;
        var h = this.model.node.parameters.h;
        var angle = this.model.node.extra.angle;

        var rVector = new THREE.Vector3(r*Math.cos(angle), r*Math.sin(angle), 0);
        var rDim = SS.createDimArrow(r, rVector);
        this.sceneObject.add(rDim);

        var hDim = SS.createDimArrow(h, new THREE.Vector3(0,0,h));
        hDim.position = rVector;
        this.sceneObject.add(hDim);

        this.sceneObject.position = new THREE.Vector3(origin.x, origin.y, origin.z);
        this.postRender();
    },

});

SS.CylinderDimensionText = SS.DimensionText.extend({
    
    render: function() {
        this.clear();
        var r = this.model.node.parameters.r;
        var h = this.model.node.parameters.h;
        this.$r = this.addElement('<div class="dimension">' + r + '</div>');
        this.$h = this.addElement('<div class="dimension">' + h + '</div>');
        this.update();
    },

    update: function() {
        var origin = this.model.node.origin;
        var r = this.model.node.parameters.r;
        var h = this.model.node.parameters.h;
        var angle = this.model.node.extra.angle;
        
        var rVector = new THREE.Vector3(r*Math.cos(angle), r*Math.sin(angle), 0);

        var rPixelPosition = SS.toScreenCoordinates(
            new THREE.Vector3(origin.x + r/2*Math.cos(angle), 
                              origin.y + r/2*Math.sin(angle), 
                              origin.z));
        this.$r.css('left', rPixelPosition.x);
        this.$r.css('top', rPixelPosition.y);

        var hPixelPosition = SS.toScreenCoordinates(
            new THREE.Vector3(origin.x + r*Math.cos(angle), 
                              origin.y + r*Math.sin(angle), 
                              origin.z + h/2));
        this.$h.css('left', hPixelPosition.x);
        this.$h.css('top', hPixelPosition.y);
    },

});