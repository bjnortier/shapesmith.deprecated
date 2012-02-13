var SS = SS || {};

SS.ScaleTransformerInitiator = SS.TransformerInitiator.extend({

    initialize: function() { 
        SS.TransformerInitiator.prototype.initialize.call(this);

        this.arrowViews = [
	    new SS.ScaleArrowViewMaxXMaxY({model: this}),
            new SS.ScaleArrowViewMaxXMinY({model: this}),
            new SS.ScaleArrowViewMinXMinY({model: this}),
            new SS.ScaleArrowViewMinXMaxY({model: this}),
	];
        
        this.views = this.views.concat(this.arrowViews);
        this.views = this.views.concat([
            new SS.ScaleBoxView({model: this}),
            new SS.ScaleFootprintView({model: this}),
        ]);
    },

    mouseDownOnArrow: function(arrowView) {
        
	var geomNode = this.attributes.geomNode;
        var editingNode = geomNode.editableCopy();
        var transform = new Transform({
            type: 'scale',
            editing: true,
	    origin: {x: parseFloat((this.center.x).toFixed(3)), 
                     y: parseFloat((this.center.y).toFixed(3)), 
                     z: 0},
            parameters: {factor: 1.0}
        });
        editingNode.transforms.push(transform);
        geomNode.originalSceneObjects = geomNode.sceneObjects;

        selectionManager.deselectID(geomNode.id);
        geom_doc.replace(geomNode, editingNode);

        new SS.ScaleTransformer({originalNode: geomNode,
                                 editingNode: editingNode, 
                                 transform: transform,
                                 anchorFunction: arrowView.anchorFunction,
                                 arrowViews: this.arrowViews});
        this.destroy();
    },

});

SS.recursiveHighlightFn =  function(object, opacity) {
    var functor = function(object) {
	if (object.material) {
	    object.material.opacity = opacity;
	}
	if (object.children) {
	    object.children.map(functor);
	}
    }
    functor(object);
};

SS.ScaleArrowView = SS.ActiveTransformerView.extend({
    
    initialize: function() {
	SS.ActiveTransformerView.prototype.initialize.call(this);
        this.on('mouseDown', this.mouseDown, this);
    },

    mouseDown: function() {
        this.model.mouseDownOnArrow && this.model.mouseDownOnArrow(this);
    },

    remove: function() {
        SS.ActiveTransformerView.prototype.remove.call(this);
        this.model.off('mouseDown', this.mouseDownOnArrow);
    },

    render: function() {
        this.clear();

        var arrowGeometry = new THREE.Geometry();
        var positions = [[0, 0, 0], 
                         [2, -1.5, 0], [2, -0.5, 0], 
                         [3, -0.5, 0], [3, -1.5, 0],
                         [5, 0, 0], 
                         [3, 1.5, 0], [3, 0.5, 0], 
                         [2, 0.5, 0], [2, 1.5, 0], 
                         [0, 0, 0]];

        arrowGeometry.vertices = positions.map(function(coordinates) {
            return new THREE.Vertex(new THREE.Vector3(coordinates[0], coordinates[1], coordinates[2]));
        });
        arrowGeometry.faces.push(new THREE.Face4(2,3,7,8));
        arrowGeometry.faces.push(new THREE.Face3(0,1,9));
        arrowGeometry.faces.push(new THREE.Face3(4,5,6));
        arrowGeometry.computeCentroids();
        arrowGeometry.computeFaceNormals();
        
        var arrowMesh = new THREE.Mesh(arrowGeometry, 
                                       new THREE.MeshBasicMaterial({color: SS.constructors.faceColor, 
                                                                    transparent: true, 
                                                                    opacity: 0.5}));
        arrowMesh.doubleSides = true;
        
        var lineGeom = new THREE.Geometry();
        lineGeom.vertices = arrowGeometry.vertices;
        var line = new THREE.Line(lineGeom, 
                                  new THREE.LineBasicMaterial({color: SS.constructors.lineColor, 
                                                               wireframe : true, 
                                                               linewidth: 2.0, 
                                                               transparent: true, 
                                                               opacity: 0.5 }));

        arrowMesh.name = {transformerElement: 'scale+X+Y'};
        line.name = {transformerElement:  'scale+X+Y'};
        
        this.sceneObject.add(arrowMesh);
        this.sceneObject.add(line);
       
        return this;
    },

    
});

SS.ScaleArrowViewMaxXMaxY = SS.ScaleArrowView.extend({
    
    initialize: function() {
	SS.ScaleArrowView.prototype.initialize.call(this);
        this.anchorFunction = function(boundingBox) {
            return {x: boundingBox.max.x, 
                    y: boundingBox.max.y};
        }
    },

    render: function() {
        SS.ScaleArrowView.prototype.render.call(this);
        this.sceneObject.position.x = this.model.boundingBox.max.x + 1;
        this.sceneObject.position.y = this.model.boundingBox.max.y + 1;
        this.sceneObject.rotation.z = 1/4*Math.PI;
        this.postRender();
        return this;
    }
});

SS.ScaleArrowViewMinXMaxY = SS.ScaleArrowView.extend({

    initialize: function() {
	SS.ScaleArrowView.prototype.initialize.call(this);
        this.anchorFunction = function(boundingBox) {
            return {x: boundingBox.min.x, 
                    y: boundingBox.max.y};
        }
    },

    render: function() {
        SS.ScaleArrowView.prototype.render.call(this);
        this.sceneObject.position.x = this.model.boundingBox.min.x - 1;
        this.sceneObject.position.y = this.model.boundingBox.max.y + 1;
        this.sceneObject.rotation.z = 3/4*Math.PI;
        this.postRender();
        return this;
    }
});

SS.ScaleArrowViewMinXMinY = SS.ScaleArrowView.extend({

    initialize: function() {
	SS.ScaleArrowView.prototype.initialize.call(this);
        this.anchorFunction = function(boundingBox) {
            return {x: boundingBox.min.x, 
                    y: boundingBox.min.y};
        }
    },

    render: function() {
        SS.ScaleArrowView.prototype.render.call(this);
        this.sceneObject.position.x = this.model.boundingBox.min.x - 1;
        this.sceneObject.position.y = this.model.boundingBox.min.y - 1;
        this.sceneObject.rotation.z = 5/4*Math.PI;
        this.postRender();
        return this;
    }
});

SS.ScaleArrowViewMaxXMinY = SS.ScaleArrowView.extend({

    initialize: function() {
	SS.ScaleArrowView.prototype.initialize.call(this);
        this.anchorFunction = function(boundingBox) {
            return {x: boundingBox.max.x, 
                    y: boundingBox.min.y};
        }
    },

    render: function() {
        SS.ScaleArrowView.prototype.render.call(this);
        this.sceneObject.position.x = this.model.boundingBox.max.x + 1;
        this.sceneObject.position.y = this.model.boundingBox.min.y - 1;
        this.sceneObject.rotation.z = 7/4*Math.PI;
        this.postRender();
        return this;
    }
});


SS.ScaleBoxView = SS.SceneObjectView.extend({

    initialize: function() {
	SS.SceneObjectView.prototype.initialize.call(this);
       
    },
    
    render: function() {
        this.clear();
        
        var width  = this.model.boundingBox.max.x - this.model.boundingBox.min.x;
        var depth  = this.model.boundingBox.max.y - this.model.boundingBox.min.y;
        var height = this.model.boundingBox.max.z - this.model.boundingBox.min.z;

        var geometry = new THREE.CubeGeometry(width, depth, height);
	cube = new THREE.Mesh(geometry, new THREE.MeshBasicMaterial({color: SS.constructors.lineColor, 
                                                                     wireframe: true}));
        
	cube.position.x = this.model.boundingBox.min.x + width/2;
	cube.position.y = this.model.boundingBox.min.y + depth/2;
	cube.position.z = this.model.boundingBox.min.z + height/2;
	this.sceneObject.add(cube);

        this.postRender();
        return this;
    },

});

SS.ScaleFootprintView = SS.SceneObjectView.extend({

    initialize: function() {
	SS.SceneObjectView.prototype.initialize.call(this);
    },
    
    render: function() {
        this.clear();
        
        var width  = this.model.boundingBox.max.x - this.model.boundingBox.min.x;
        var depth  = this.model.boundingBox.max.y - this.model.boundingBox.min.y;
        var height = this.model.boundingBox.max.z - this.model.boundingBox.min.z;

        var planeGeometry = new THREE.PlaneGeometry(width, depth); 
        var planeMesh = THREE.SceneUtils.createMultiMaterialObject(
            planeGeometry, 
            [
                new THREE.MeshBasicMaterial({color: SS.constructors.lineColor, 
                                             wireframe: true}),
                new THREE.MeshBasicMaterial({color: SS.constructors.faceColor, 
                                             transparent: true, 
                                             opacity: 0.5})
            ]);

        planeMesh.doubleSided = true;
        planeMesh.position.x = this.model.boundingBox.min.x + width/2;
        planeMesh.position.y = this.model.boundingBox.min.y + depth/2;
        planeMesh.position.z = -0.05;
	this.sceneObject.add(planeMesh);

        this.postRender();
        return this;
    },

});
