var SS = SS || {};

SS.CuboidCreator = SS.PrimitiveCreator.extend({

    initialize: function(attributes) {
        SS.PrimitiveCreator.prototype.initialize.call(this, attributes);

        this.node.parameters.u = 10;
        this.node.parameters.v = 10;
        this.node.parameters.w = 10;

        this.views = this.views.concat([
            new SS.CuboidPreview({model: this}),
            new SS.DraggableUVCorner({model: this}),
            new SS.CuboidDimensionArrows({model: this}),
            new SS.CuboidDimensionText({model: this}),
        ]);
        this.trigger('change', this);
    },

    mouseDownOnUV: function(corner) {
        this.activateCorner(corner, SS.UVWHeightCursoid);
    },

    getBoundingBox: function() {
        var origin = this.node.origin;
        var u = this.node.parameters.u;
        var v = this.node.parameters.v;
        var w = this.node.parameters.w;
        return {min: new THREE.Vector3(origin.x, origin.y, origin.z),
                max: new THREE.Vector3(origin.x + u, origin.y + v, origin.z + w)};
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
	    cube.position = new THREE.Vector3(u/2, v/2, w/2);
	    this.sceneObject.add(cube);

            if (origin.z) {
                var baseGeom = new THREE.PlaneGeometry(u, v);
                var base = new THREE.Mesh(baseGeom, SS.constructors.wireframeMaterial);
                base.position = new THREE.Vector3(u/2, v/2, -origin.z);
	        this.sceneObject.add(base);
            }

        } else if (u && v) {
            var planeGeom = new THREE.PlaneGeometry(u, v);
            var plane = THREE.SceneUtils.createMultiMaterialObject(planeGeom, materials);
            plane.position = new THREE.Vector3(u/2, v/2, 0);
            
	    this.sceneObject.add(plane);
        }


        this.postRender();
    },

});


SS.DraggableUVCorner = SS.DraggableCorner.extend({

    initialize: function(options) {
        SS.DraggableCorner.prototype.initialize.call(this, options);
        this.uKey = options.uKey || 'u';
        this.vKey = options.vKey || 'v';
        this.render();
    },

    priority: 1,

    mouseDown: function() {
        this.model.mouseDownOnUV(this);
    },

    cornerPositionFromModel: function() {
        return {x: this.model.node.origin.x + this.model.node.parameters[this.uKey],
                y: this.model.node.origin.y + this.model.node.parameters[this.vKey],
                z: 0};
    },

    updateModelFromCorner: function(position) {
        var u = position.x - this.model.node.origin.x;
        var v = position.y - this.model.node.origin.y;

        this.model.node.parameters[this.uKey] = Math.round(u);
        this.model.node.parameters[this.vKey] = Math.round(v);
    },

});

SS.UVWHeightCursoid = SS.HeightCursoid.extend({

     initialize: function(options) {
	 SS.HeightCursoid.prototype.initialize.call(this);
         this.uKey = options.uKey || 'u';
         this.render();
    },

    cornerPositionFromModel: function() {
        var parameters = this.model.node.parameters
        return {x: this.model.node.origin.x + this.model.node.parameters[this.uKey],
                y: this.model.node.origin.y + this.model.node.parameters.v,
                z: this.model.node.origin.z + this.model.node.parameters.w};
    },    

    updateModelFromCorner: function(position) {
        var parameters = this.model.node.parameters
        var origin = this.model.node.origin;
        parameters[this.uKey] = position.x - origin.x;
        parameters.v = position.y - origin.y;
        parameters.w = position.z - origin.z;
    },

});

SS.CuboidDimensionArrows = SS.SceneObjectView.extend({

    render: function() {
        this.clear();

        var origin = this.model.node.origin;
        var u = this.model.node.parameters.u;
        var v = this.model.node.parameters.v;
        var w = this.model.node.parameters.w;

        var uDim = SS.createDimArrow(u, new THREE.Vector3(u,0,0));
        var vDim = SS.createDimArrow(v, new THREE.Vector3(0,v,0));
        var wDim = SS.createDimArrow(v, new THREE.Vector3(0,0,w));
        vDim.position = new THREE.Vector3(u,0,0);
        wDim.position = new THREE.Vector3(u,v,0);
        this.sceneObject.add(uDim);
        this.sceneObject.add(vDim);
        this.sceneObject.add(wDim);

        this.sceneObject.position = new THREE.Vector3(origin.x, origin.y, origin.z);

        this.postRender();
    },

});


SS.CuboidDimensionText = SS.DimensionText.extend({

    render: function() {
        this.clear();

        var origin = this.model.node.origin;
        var u = this.model.node.parameters.u;
        var v = this.model.node.parameters.v;
        var w = this.model.node.parameters.w;

        var that = this;
        ['u', 'v', 'w'].map(function(dim) {
            var key = '$' + dim;
            that[key] = that.addElement('<div class="dimension">' + that.model.node.parameters[dim] + '</div>');
        });

        this.update();
    },

    update: function() {
        var origin = this.model.node.origin;
        var u = this.model.node.parameters.u;
        var v = this.model.node.parameters.v;
        var w = this.model.node.parameters.w;
      
        var positions = {u: new THREE.Vector3(origin.x + u/2, origin.y, origin.z),
                         v: new THREE.Vector3(origin.x + u, origin.y + v/2, origin.z),
                         w: new THREE.Vector3(origin.x+ u, origin.y + v, origin.z + w/2)};
        for (key in positions) {
            var pixelPosition = SS.toScreenCoordinates(positions[key]);
            this['$' + key].css('left', pixelPosition.x);
            this['$' + key].css('top', pixelPosition.y);
        }
    },

});