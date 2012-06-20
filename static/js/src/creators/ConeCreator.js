var SS = SS || {};

SS.ConeCreator = SS.PrimitiveCreator.extend({

    initialize: function(attributes) {
        SS.PrimitiveCreator.prototype.initialize.call(this, attributes);

        this.node.extra = {angle: 0};
        this.views = this.views.concat([
            new SS.ConePreview({model: this}),
            new SS.DraggableRadiusCorner({model: this, key: 'r1'}),
            new SS.ConeDimensionArrows({model: this}),
            new SS.ConeDimensionText({model: this}),
        ]);
        this.trigger('change', this);
    },

    setDefaultParamters: function() {
        this.node.parameters.r1 = 10;
        this.node.parameters.r2 = 0;
        this.node.parameters.h = 10;
    },

    mouseDownOnRadius: function(corner) {
        this.activateCorner(corner, SS.RadiusHeightCursoid, {model: this, key: 'r1'});
    },

    getBoundingBox: function() {
        var origin = this.node.origin;
        var r1 = this.node.parameters.r1;
        var h = this.node.parameters.h;
        return {min: new THREE.Vector3(origin.x - r1, origin.y - r1, origin.z),
                max: new THREE.Vector3(origin.x + r1, origin.y + r1, origin.z + h)};
    },


});

SS.ConePreview = SS.PreviewWithOrigin.extend({

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

        if (r1 && h) {
            var geometry = new THREE.CylinderGeometry(r1, r2 || 0.000001, h, 20);
	    var cone = new THREE.Mesh(geometry, SS.materials.faceMaterial);
	    cone.position.z = h/2;
            cone.rotation.x = -Math.PI/2;
	    this.sceneObject.add(cone);
	    
	    var circleGeom2 = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
		var theta = Math.PI*2*i/50;
		var dx = r2*Math.cos(theta);
		var dy = r2*Math.sin(theta);
		circleGeom2.vertices.push(new THREE.Vector3(dx, dy, 0));
	    }
	    var circle2 = new THREE.Line(circleGeom2, SS.materials.lineMaterial);
            circle2.position.z = h;
	    this.sceneObject.add(circle2);

	}

        if (r1 && !h) {
            var ellipseGeom = new THREE.EllipseGeometry(r1, r1);
	    var ellipseFace = new THREE.Mesh(ellipseGeom, SS.materials.faceMaterial);
            ellipseFace.doubleSided = true;
            this.sceneObject.add(ellipseFace);
        }

        if (r1) {
	    var circleGeom1 = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
		var theta = Math.PI*2*i/50;
		var dx = r1*Math.cos(theta);
		var dy = r1*Math.sin(theta);
		circleGeom1.vertices.push(new THREE.Vector3(dx, dy, 0));
	    }
	    var circle1 = new THREE.Line(circleGeom1, SS.materials.lineMaterial);
	    this.sceneObject.add(circle1);

            if (origin.z) {
	        var circle3 = new THREE.Line(circleGeom1, SS.materials.lineMaterial);
                circle3.position.z = -origin.z;
	        this.sceneObject.add(circle3);

            }
        }
        
       
        this.postRender();
    },

});


SS.ConeDimensionArrows = SS.DimensionArrowsView.extend({

    render: function() {
        this.clear();

        var origin = this.model.node.origin;
        var r1 = this.model.node.parameters.r1;
        var r2 = this.model.node.parameters.r2;
        var h = this.model.node.parameters.h;
        var angle = this.model.node.extra.angle;

        var r1Vector = new THREE.Vector3(r1*Math.cos(angle), r1*Math.sin(angle), 0);
        var r1Dim = this.createDimArrow(r1, r1Vector);
        this.sceneObject.add(r1Dim);

        if (r2) {
            var r2Vector = new THREE.Vector3(r2*Math.cos(angle), r2*Math.sin(angle), 0);
            var r2Dim = this.createDimArrow(r2, r2Vector);
            r2Dim.position = new THREE.Vector3(0,0,h);
            this.sceneObject.add(r2Dim);
        }

        var hDim = this.createDimArrow(h, new THREE.Vector3(0,0,h));
        this.sceneObject.add(hDim);

        this.sceneObject.position = new THREE.Vector3(origin.x, origin.y, origin.z);
        this.postRender();
    },

});

SS.ConeDimensionText = SS.DimensionText.extend({
    
    render: function() {
        this.clear();
        var r1 = this.model.node.parameters.r1;
        var r2 = this.model.node.parameters.r2;
        var h = this.model.node.parameters.h;
        this.$r1 = this.addElement('<div class="dimension">' + r1 + '</div>');
        this.$h = this.addElement('<div class="dimension">' + h + '</div>');
        if (r2) {
            this.$r2 = this.addElement('<div class="dimension">' + r2 + '</div>');
        }
        this.update();
    },

    update: function() {
        var origin = this.model.node.origin;
        var r1 = this.model.node.parameters.r1;
        var r2 = this.model.node.parameters.r2;
        var h = this.model.node.parameters.h;
        var angle = this.model.node.extra.angle;
        
        var r1Vector = new THREE.Vector3(r1*Math.cos(angle), r1*Math.sin(angle), 0);

        this.moveToScreenCoordinates(this.$r1,
                                     new THREE.Vector3(origin.x + r1/2*Math.cos(angle), 
                                                       origin.y + r1/2*Math.sin(angle), 
                                                       origin.z));

        if (this.$r2) {
            this.moveToScreenCoordinates(this.$r2, 
                                         new THREE.Vector3(origin.x + r2/2*Math.cos(angle), 
                                                           origin.y + r2/2*Math.sin(angle), 
                                                           origin.z + h));
        }

        this.moveToScreenCoordinates(this.$h, 
                                     new THREE.Vector3(origin.x, 
                                                       origin.y, 
                                                       origin.z + h/2));
    },

});
