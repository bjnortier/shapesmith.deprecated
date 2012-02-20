var SS = SS || {};

SS.Rectangle2DCreator = SS.PrimitiveCreator.extend({

    initialize: function(attributes) {
        SS.PrimitiveCreator.prototype.initialize.call(this, attributes);

        this.views = this.views.concat([
            new SS.Rectangle2DPreview({model: this}),
            new SS.DraggableUVCorner({model: this}),
            new SS.Rectangle2DDimensionArrows({model: this}),
            new SS.Rectangle2DDimensionText({model: this}),

        ]);
        this.trigger('change', this);
    },

    setDefaultParamters: function() {
        this.node.parameters.u = 10;
        this.node.parameters.v = 10;
    },

    mouseDownOnUV: function(corner) {
        this.activateCorner(corner);
    },

    getBoundingBox: function() {
        var origin = this.node.origin;
        var u = this.node.parameters.u;
        var v = this.node.parameters.v;
        return {min: new THREE.Vector3(origin.x, origin.y, origin.z),
                max: new THREE.Vector3(origin.x + u, origin.y + v, origin.z)};
    },


});

SS.Rectangle2DPreview = SS.PreviewWithOrigin.extend({

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

        var materials = [ SS.constructors.faceMaterial, SS.constructors.wireframeMaterial ];

        if (u && v) {
            var planeGeom = new THREE.PlaneGeometry(u, v);
            var planeFace = new THREE.Mesh(planeGeom,  SS.constructors.faceMaterial);
            planeFace.doubleSided = true;

            var planeWire = new THREE.Mesh(planeGeom,  SS.constructors.wireframeMaterial);

            planeFace.position = new THREE.Vector3(u/2, v/2, 0);
            planeWire.position = planeFace.position;
            
	    this.sceneObject.add(planeFace);
	    this.sceneObject.add(planeWire);

            if (origin.z !== 0) {
	        var rectangleBaseWire = new THREE.Mesh(planeGeom,  SS.constructors.wireframeMaterial);
                rectangleBaseWire.position = new THREE.Vector3(u/2, v/2, -origin.z);
                this.sceneObject.add(rectangleBaseWire);
            }

        }

        this.postRender();
    },

});

SS.Rectangle2DDimensionArrows = SS.SceneObjectView.extend({

    render: function() {
        this.clear();

        var origin = this.model.node.origin;
        var u = this.model.node.parameters.u;
        var v = this.model.node.parameters.v;

        var uDim = SS.createDimArrow(u, new THREE.Vector3(u,0,0));
        var vDim = SS.createDimArrow(v, new THREE.Vector3(0,v,0));
        vDim.position = new THREE.Vector3(u,0,0);
        this.sceneObject.add(uDim);
        this.sceneObject.add(vDim);

        this.sceneObject.position = new THREE.Vector3(origin.x, origin.y, origin.z);

        this.postRender();
    },

});

SS.Rectangle2DDimensionText = SS.DimensionText.extend({

    render: function() {
        this.clear();

        var origin = this.model.node.origin;
        var u = this.model.node.parameters.u;
        var v = this.model.node.parameters.v;

        var that = this;
        ['u', 'v'].map(function(dim) {
            var key = '$' + dim;
            that[key] = that.addElement('<div class="dimension">' + that.model.node.parameters[dim] + '</div>');
        });

        this.update();
    },

    update: function() {
        var origin = this.model.node.origin;
        var u = this.model.node.parameters.u;
        var v = this.model.node.parameters.v;
      
        var positions = {u: new THREE.Vector3(origin.x + u/2, origin.y, origin.z),
                         v: new THREE.Vector3(origin.x + u, origin.y + v/2, origin.z)};
        for (key in positions) {
            var pixelPosition = SS.toScreenCoordinates(positions[key]);
            this['$' + key].css('left', pixelPosition.x);
            this['$' + key].css('top', pixelPosition.y);
        }
    },

});

