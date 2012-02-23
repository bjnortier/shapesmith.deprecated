var SS = SS || {};

SS.RotateTransformerInitiator = SS.TransformerInitiator.extend({

    initialize: function(attributes) { 
        SS.TransformerInitiator.prototype.initialize.call(this, attributes);
        
        this.arrowViews = [
	    new SS.RotateArrowViewX({model: this}),
            new SS.RotateArrowViewY({model: this}),
            new SS.RotateArrowViewZ({model: this}),
	];
        
        this.views = this.views.concat(this.arrowViews);
        this.views = this.views.concat([
            new SS.RotateAxesView({model: this}),
        ]);
    },

    mouseDownOnArrow: function(arrowView) {
        
	var geomNode = this.geomNode;
        var editingNode = geomNode.editableCopy();
        var parameters = {u: arrowView.axis.x,
                          v: arrowView.axis.y,
                          w: arrowView.axis.z,
                          angle: 0,
                          n: 0};
                
        var transform = new Transform({
            type: 'rotate',
            editing: true,
	    origin: {x: Math.round(this.center.x), 
                     y: Math.round(this.center.y), 
                     z: Math.round(this.center.z)},
            parameters: parameters
        });
        editingNode.transforms.push(transform);
        geomNode.originalSceneObjects = geomNode.sceneObjects;

        selectionManager.deselectID(geomNode.id);
        geom_doc.replace(geomNode, editingNode);

        new SS.RotateTransformer({originalNode: geomNode,
                                  editingNode: editingNode, 
                                  transform: transform,
                                  anchorPosition: arrowView.anchorFunction(),
                                  rotationPlane: arrowView.rotationPlane,
                                  mouseDownArrowViewIndex: this.arrowViews.indexOf(arrowView)});
        this.destroy();
    },

});


SS.RotateTransformer = SS.Transformer.extend({

    initialize: function(attributes) { 
        SS.Transformer.prototype.initialize.call(this, attributes);

        if (!attributes.editingExisting) {
            this.anchorPosition = attributes.anchorPosition;
            var arrowViews = [
	        new SS.RotateArrowViewX({model: this}),
                new SS.RotateArrowViewY({model: this}),
                new SS.RotateArrowViewZ({model: this}),
	    ];
            SS.sceneView.addToMouseOverAndMouseDown(arrowViews[attributes.mouseDownArrowViewIndex]);

            var newViews = [
                new SS.RotateGeomNodeView({model: this}),
                new SS.RotateAxesView({model: this}),
                new SS.RotateAngleView({model: this}),
                new SS.RotationAngleText({model: this}),
            ];

            this.views = this.views.concat(newViews);
            this.views = this.views.concat(arrowViews);
        }
    },

    mouseDownOnArrow: function(arrowView) {
        this.anchorPosition = arrowView.anchorFunction();
        this.rotationPlane  = arrowView.rotationPlane;
    },


});

SS.RotateGeomNodeView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change:model', this.render, this);
    },

    render: function() {
        var transform = this.model.transform;
        var center = new THREE.Vector3(transform.origin.x,
                                       transform.origin.y,
                                       transform.origin.z);
        
        var axis = new THREE.Vector3(transform.parameters.u,
                                     transform.parameters.v,
                                     transform.parameters.w);

        SS.rotateGeomNodeRendering(this.model.originalNode, 
                                   this.model.editingNode, 
                                   center,
                                   axis,
                                   this.model.transform.parameters.angle);
    },

});

SS.RotateAxesView = SS.InteractiveSceneView.extend({

    initialize: function() {
        SS.InteractiveSceneView.prototype.initialize.call(this);
        this.render();
    },

    render: function() {
        this.clear();

        var that = this;
        ['x', 'y', 'z'].map(function(key) {
            var lineGeom = new THREE.Geometry();
            var from = that.model.center.clone(), to = that.model.center.clone();
            from[key] = that.model.boundingBox.min[key] - 5;
            to[key]   = that.model.boundingBox.max[key] + 5;

	    lineGeom.vertices.push(new THREE.Vertex(from), new THREE.Vertex(to));
	    var line = new THREE.Line(lineGeom, SS.constructors.lineMaterial);
            that.sceneObject.add(line);
        });

        if (this.model.transform) {

            var transform = this.model.transform;
            var origin =transform.origin;
            var u = transform.parameters.u;
            var v = transform.parameters.v;
            var w = transform.parameters.w;
            var axisVector = new THREE.Vector3(u,v,w).normalize();
            
            var axis = new THREE.Geometry();
            axis.vertices.push(new THREE.Vertex(axisVector.clone().multiplyScalar(1000)));
            axis.vertices.push(new THREE.Vertex(axisVector.clone().multiplyScalar(-1000)));
            var line = new THREE.Line(axis, SS.constructors.lineMaterial);  
            line.position = new THREE.Vector3(origin.x, origin.y, origin.z);
            this.sceneObject.add(line);
        }

        this.postRender();
    },

});

SS.RotateArrowView = SS.InteractiveSceneView.extend({

    initialize: function() {
	SS.InteractiveSceneView.prototype.initialize.call(this);
        this.on('mouseDown', this.mouseDown, this);

        this.rotationPlane = new THREE.Mesh(
            new THREE.PlaneGeometry(1000, 1000),
            new THREE.MeshBasicMaterial({ color: 0x333366, opacity: 0.0, transparent: true }));
        this.rotationPlane.position = this.model.center;
        this.rotationPlane.doubleSided = true;

        this.on('mouseDrag', this.drag);

        SS.sceneView.scene.add(this.rotationPlane);
    },

    remove: function() {
        SS.InteractiveSceneView.prototype.remove.call(this);
        this.off('mouseDown', this.mouseDown);
        this.off('mouseDrag', this.drag);
        SS.sceneView.scene.remove(this.rotationPlane);
        
    },

    mouseDown: function() {
        this.model.mouseDownOnArrow && this.model.mouseDownOnArrow(this);
    },

    drag: function(event) {
        var positionOnRotationPlane = SS.sceneView.determinePositionOnPlane(event, this.rotationPlane);
        if (!positionOnRotationPlane) {
            return;
        }

        this.model.currentArrowView = this;

        var v1 = new THREE.Vector3().sub(
            new THREE.Vector3(
                positionOnRotationPlane.x, 
                positionOnRotationPlane.y, 
                positionOnRotationPlane.z),
            this.model.center).normalize();
        var v2 = new THREE.Vector3().sub(this.model.anchorPosition,
                                         this.model.center).normalize();
        var v2CrossV1 = new THREE.Vector3().cross(v2, v1);
        var rotationVector = this.axis;

        var angle = parseFloat((Math.acos(v1.dot(v2))/Math.PI*180).toFixed(0));
        if (rotationVector.dot(v2CrossV1) < 0) {
            angle = -angle;
        }
        if (!event.ctrlKey) {
            angle = Math.round(angle/5)*5;
        }
        
        this.model.setParameters({
            u: this.axis.x,
            v: this.axis.y,
            w: this.axis.z,
            angle: angle,
        });
    },
    
    render: function() {
        this.clear();

        var width = 0.5;
        var r = 20;
        var dr = 0.3;
        var maxAngle = 4;

        var outerCurveGeometry = new THREE.Geometry();
        for (var i = -maxAngle; i <= 0; ++i) {
            var angle = i/180*Math.PI;
            var z = (r+dr)*Math.cos(angle);
            var x = (r+dr)*Math.sin(angle);
            outerCurveGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(x,0,z)));
        }

        var innerCurveGeometry = new THREE.Geometry();
        for (var i = 0; i >= -maxAngle; --i) {
            var angle = i/180*Math.PI;
            var z = (r-dr)*Math.cos(angle);
            var x = (r-dr)*Math.sin(angle);
            innerCurveGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(x,0,z)));
        }

        var arrowHeadGeometry = new THREE.Geometry();
        arrowHeadGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,r+dr)));
        arrowHeadGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,r+dr+width)));
        arrowHeadGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(1.5,0,r)));
        arrowHeadGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,r-dr-width)));
        arrowHeadGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,r-dr)));

        var arrowLineGeometry = new THREE.Geometry();
        arrowLineGeometry.vertices.push(innerCurveGeometry.vertices[maxAngle]);
        arrowLineGeometry.vertices.push(outerCurveGeometry.vertices[0]);
        arrowLineGeometry.vertices = arrowLineGeometry.vertices.concat(outerCurveGeometry.vertices);
        arrowLineGeometry.vertices = arrowLineGeometry.vertices.concat(arrowHeadGeometry.vertices);
        arrowLineGeometry.vertices = arrowLineGeometry.vertices.concat(innerCurveGeometry.vertices);

        var lineMaterial = new THREE.LineBasicMaterial({color: SS.constructors.lineColor, 
                                                        linewidth: 2.0, 
                                                        transparent: true, 
                                                        opacity: 0.5 });
        
        var arrowLine = new THREE.Line(arrowLineGeometry, lineMaterial);
        
        var arrowFaceGeometry = new THREE.Geometry();
        arrowFaceGeometry.vertices = outerCurveGeometry.vertices;
        arrowFaceGeometry.vertices = arrowFaceGeometry.vertices.concat(innerCurveGeometry.vertices.reverse());
        for (var i = 0; i < maxAngle; ++i) {
            arrowFaceGeometry.faces.push(new THREE.Face4(i, i+1, 
                                                         outerCurveGeometry.vertices.length + i + 1,
                                                         outerCurveGeometry.vertices.length + i));
        }
        arrowFaceGeometry.vertices.push(arrowHeadGeometry.vertices[1]);
        arrowFaceGeometry.vertices.push(arrowHeadGeometry.vertices[2]);
        arrowFaceGeometry.vertices.push(arrowHeadGeometry.vertices[3]);
        arrowFaceGeometry.faces.push(new THREE.Face3(arrowFaceGeometry.vertices.length-3,
                                                     arrowFaceGeometry.vertices.length-2,
                                                     arrowFaceGeometry.vertices.length-1));
        arrowFaceGeometry.computeCentroids();
        arrowFaceGeometry.computeFaceNormals();

        var arrowFace = new THREE.Mesh(arrowFaceGeometry,
                                       new THREE.MeshBasicMaterial({color: SS.constructors.faceColor, 
                                                                    transparent: true, 
                                                                    opacity: 0.5}));
        arrowFace.doubleSided = true;

        arrowFace.position.z = -r;
        arrowLine.position.z = -r;

        arrowFace.rotation.y = maxAngle/180*Math.PI;
        arrowLine.rotation.y = maxAngle/180*Math.PI;

        this.sceneObject.add(arrowFace);
        this.sceneObject.add(arrowLine);

        return this;
    },

});

SS.RotateArrowViewX = SS.RotateArrowView.extend({

    initialize: function() {
	SS.RotateArrowView.prototype.initialize.call(this);
        this.rotationPlane.rotation.y = Math.PI/2;
        this.render();

    },

    anchorFunction: function() {
        return new THREE.Vector3(this.model.center.x,
                                 this.model.boundingBox.max.y + 5,
                                 this.model.center.z);
                
    },

    render: function() {
        SS.RotateArrowView.prototype.render.call(this);

        this.sceneObject.position = this.anchorFunction();
        this.sceneObject.rotation.z = -Math.PI/2,
        this.sceneObject.rotation.x =  -Math.PI/2

        this.postRender();
        return this;
    },

    axis: new THREE.Vector3(1, 0, 0),
    
});

SS.RotateArrowViewY = SS.RotateArrowView.extend({

    initialize: function() {
	SS.RotateArrowView.prototype.initialize.call(this);
        this.rotationPlane.rotation.x = Math.PI/2;
        this.render();
    },

    anchorFunction: function() {
        return new THREE.Vector3(this.model.center.x,
                                 this.model.center.y,
                                 this.model.boundingBox.max.z + 5);
    },
    
    render: function() {
        SS.RotateArrowView.prototype.render.call(this);

        this.sceneObject.position = this.anchorFunction();
        this.postRender();
        return this;
    },

    axis: new THREE.Vector3(0, 1, 0),

});

SS.RotateArrowViewZ = SS.RotateArrowView.extend({

    initialize: function() {
	SS.RotateArrowView.prototype.initialize.call(this);
        this.render();
    },

    anchorFunction: function() {
        return new THREE.Vector3(this.model.boundingBox.max.x + 5,
                                 this.model.center.y,
                                 this.model.center.z);
    },
    
    render: function() {
        SS.RotateArrowView.prototype.render.call(this);

        this.sceneObject.position = this.anchorFunction();
        this.sceneObject.rotation.z = Math.PI/2,
        this.sceneObject.rotation.y = Math.PI/2
        this.postRender();
        return this;
    },

    axis: new THREE.Vector3(0, 0, 1),
    
});

SS.RotateAngleView = SS.SceneObjectView.extend({

    initialize: function() {
        SS.SceneObjectView.prototype.initialize.call(this);
        this.render();
    },
    
    render: function() {
        this.clear();

        var params = this.model.transform.parameters;
        if (!(((params.u !== 0) && (params.v === 0) && (params.w === 0)) ||
              ((params.u === 0) && (params.v !== 0) && (params.w === 0)) ||
              ((params.u === 0) && (params.v === 0) && (params.w !== 0)))) {
            return;
        }

        var rotationAngle = new THREE.Object3D();
        
        var angleGeometry = new THREE.Geometry();
        angleGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,0)));

        var r = new THREE.Vector3().sub(this.model.anchorPosition, 
                                        this.model.center).length();

        var angle = this.model.transform.parameters.angle || 0;
        for (var i = 0; 
             angle > 0 ? i <= Math.round(angle) : i > Math.round(angle); 
             angle > 0 ? ++i : --i) {
            var theta = i/180*Math.PI;
            var z = r*Math.cos(theta);
            var x = r*Math.sin(theta);
            angleGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(x,0,z)));
        }
        angleGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,0)));
        
        var lineMaterial = new THREE.LineBasicMaterial({color: 0xeeeeee, 
                                                        linewidth: 2.0, 
                                                        transparent: true, 
                                                        opacity: 1.0 });
        
        var line = new THREE.Line(angleGeometry, lineMaterial);
        line.position.z = -r;
        rotationAngle.add(line);



        this.sceneObject.add(rotationAngle);

        this.sceneObject.position = this.model.anchorPosition;
        this.sceneObject.rotation.x = 0;
        this.sceneObject.rotation.y = 0;
        this.sceneObject.rotation.z = 0;
        if (params.u !== 0) {
            this.sceneObject.rotation.z = -Math.PI/2,
            this.sceneObject.rotation.x =  -Math.PI/2
        } else if (params.w !== 0) {
            this.sceneObject.rotation.z = Math.PI/2,
            this.sceneObject.rotation.y = Math.PI/2
        }

        this.postRender();
    },

});

SS.RotationAngleText = SS.DimensionText.extend({

    render: function() {
        this.clear();
        var angle = this.model.transform.parameters.angle;
        this.$angle = this.addElement('<div class="dimension">' + angle + '</div>');
        this.update();
    },

    update: function() {
        var angle = this.model.transform.parameters.angle;
        var axis = new THREE.Vector3(this.model.transform.parameters.u,
                                     this.model.transform.parameters.v,
                                     this.model.transform.parameters.w).normalize();
        var un = axis.x, vn = axis.y, wn = axis.z;

        var anchorVector = new THREE.Vector3().sub(this.model.anchorPosition,
                                               this.model.center);
        var r = anchorVector.length();
        var position = anchorVector.normalize().clone();
        position.multiplyScalar(r+2);

        var a = (un*position.x + vn*position.y + wn*position.z);
        var x2 = a*un + (position.x - a*un)*Math.cos(angle/180*Math.PI) 
            + (vn*position.z - wn*position.y)*Math.sin(angle/180*Math.PI);
        var y2 = a*vn + (position.y - a*vn)*Math.cos(angle/180*Math.PI) 
            + (wn*position.x - un*position.z)*Math.sin(angle/180*Math.PI);
        var z2 = a*wn + (position.z - a*wn)*Math.cos(angle/180*Math.PI) 
            + (un*position.y - vn*position.x)*Math.sin(angle/180*Math.PI);
        
        this.moveToScreenCoordinates(this.$angle, 
	                             new THREE.Vector3(this.model.center.x + x2, 
                                                       this.model.center.y + y2, 
                                                       this.model.center.z + z2));
        

    },

});