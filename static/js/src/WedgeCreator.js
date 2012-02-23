var SS = SS || {};

SS.WedgeCreator = SS.PrimitiveCreator.extend({

    initialize: function(attributes) {
        SS.PrimitiveCreator.prototype.initialize.call(this, attributes);

        this.views = this.views.concat([
            new SS.WedgePreview({model: this}),
            new SS.DraggableUVCorner({model: this, uKey: 'u2'}),
            new SS.DraggableUCorner({model: this, uKey: 'u1'}),
            new SS.WedgeDimensionArrows({model: this}),
            new SS.WedgeDimensionText({model: this}),
        ]);
        this.trigger('change', this);
    },

    setDefaultParamters: function() {
        this.node.parameters.u1 = 10;
        this.node.parameters.u2 = 5;
        this.node.parameters.v = 10;
        this.node.parameters.w = 10;
    },

    mouseDownOnUV: function(corner) {
        this.activateCorner(corner, SS.UVWHeightCursoid, {model: this, uKey: 'u2'});
    },

    mouseDownOnU: function(corner) {
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
	    wedge.position.x = u1/2;
	    wedge.position.y = v/2;
	    wedge.position.z = w/2;
            this.sceneObject.add(wedge);

        } 

        if (u1 && v && u2) {

	    var uvLineGeom = new THREE.Geometry();
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u1, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u2, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));

            if (!w) {
	        var uvLine = new THREE.Line(uvLineGeom, SS.constructors.lineMaterial);
	        this.sceneObject.add(uvLine);
            }

            if (origin.z) {
                var uvBase = new THREE.Line(uvLineGeom, SS.constructors.lineMaterial);
                uvBase.position.z = -origin.z;
	        this.sceneObject.add(uvBase);                
            }
        }

        this.postRender();
    },

});

SS.DraggableUCorner = SS.DraggableCorner.extend({

    initialize: function(options) {
        SS.DraggableCorner.prototype.initialize.call(this, options);
        this.uKey = options.uKey || 'u1';
        this.render();
    },

    priority: 2,

    mouseDown: function() {
        this.model.mouseDownOnU(this);
    },

    cornerPositionFromModel: function() {
        return {x: this.model.node.origin.x + this.model.node.parameters[this.uKey],
                y: this.model.node.origin.y + this.model.node.parameters.v,
                z: 0};
    },

    updateModelFromCorner: function(position) {
        var u = position.x - this.model.node.origin.x;
        this.model.node.parameters[this.uKey] = Math.round(u);
    },

});

SS.WedgeDimensionArrows = SS.SceneObjectView.extend({

    render: function() {
        this.clear();

        var origin = this.model.node.origin;
        var u1 = this.model.node.parameters.u1;
        var u2 = this.model.node.parameters.u2;
        var v = this.model.node.parameters.v;
        var w = this.model.node.parameters.w;

        var u1Dim = SS.createDimArrow(u1, new THREE.Vector3(u1,0,0));
        var u2Dim = SS.createDimArrow(u2, new THREE.Vector3(u2,0,0));
        var vDim = SS.createDimArrow(v, new THREE.Vector3(0,v,0));
        var wDim = SS.createDimArrow(v, new THREE.Vector3(0,0,w));
        u2Dim.position = new THREE.Vector3(0,v,0);
        vDim.position = new THREE.Vector3(0,0,0);
        wDim.position = new THREE.Vector3(u2,v,0);
        this.sceneObject.add(u1Dim);
        this.sceneObject.add(u2Dim);
        this.sceneObject.add(vDim);
        this.sceneObject.add(wDim);

        this.sceneObject.position = new THREE.Vector3(origin.x, origin.y, origin.z);

        this.postRender();
    },

});

SS.WedgeDimensionText = SS.DimensionText.extend({

    render: function() {
        this.clear();

        var origin = this.model.node.origin;
        var u1 = this.model.node.parameters.u1;
        var u2 = this.model.node.parameters.u2;
        var v = this.model.node.parameters.v;
        var w = this.model.node.parameters.w;

        var that = this;
        ['u1', 'u2', 'v', 'w'].map(function(dim) {
            var key = '$' + dim;
            that[key] = that.addElement('<div class="dimension">' + that.model.node.parameters[dim] + '</div>');
        });

        this.update();
    },

    update: function() {
        var origin = this.model.node.origin;
        var u1 = this.model.node.parameters.u1;
        var u2 = this.model.node.parameters.u2;
        var v = this.model.node.parameters.v;
        var w = this.model.node.parameters.w;
      
        var positions = {u1: new THREE.Vector3(origin.x + u1/2, origin.y, origin.z),
                         u2: new THREE.Vector3(origin.x + u2/2, origin.y + v, origin.z),
                         v: new THREE.Vector3(origin.x, origin.y + v/2, origin.z),
                         w: new THREE.Vector3(origin.x + u2, origin.y + v, origin.z + w/2)};
        for (key in positions) {
            this.moveToScreenCoordinates(this['$' + key], positions[key]);
        }
    },

});
