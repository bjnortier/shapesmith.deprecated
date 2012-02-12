var SS = SS || {};

SS.ScaleTransformInitiatorModel = Backbone.Model.extend({

    initialize: function() { 
        this.boundingBox = SS.boundingBoxForGeomNode(this.attributes.geomNode);
        this.views = [
	    new SS.ScaleTransformArrowViewMaxXMaxY({model: this}),
            new SS.ScaleTransformArrowViewMaxXMinY({model: this}),
            new SS.ScaleTransformArrowViewMinXMinY({model: this}),
            new SS.ScaleTransformArrowViewMinXMaxY({model: this}),
	];

        var that = this;
        selectionManager.on('deselected', this.deselected, this);
        selectionManager.on('selected', this.selected, this);

    },

    mouseDownOnArrow: function(arrowView, event) {
        
	var geomNode = this.attributes.geomNode;
	var boundingBox = SS.boundingBoxForGeomNode(geomNode);
        var center = SS.transformers.centerOfGeom(boundingBox);
        
        var editingNode = geomNode.editableCopy();
        var transform = new Transform({
            type: 'scale',
            editing: true,
	    origin: {x: parseFloat((center.x).toFixed(3)), 
                     y: parseFloat((center.y).toFixed(3)), 
                     z: 0},
            parameters: {factor: 1.0}
        });
        editingNode.transforms.push(transform);
        geomNode.originalSceneObjects = geomNode.sceneObjects;

        selectionManager.deselectID(geomNode.id);
        geom_doc.replace(geomNode, editingNode);

        new SS.ScaleTransformerModel({originalNode: geomNode,
                                      editingNode: editingNode, 
                                      transform: transform,
                                      anchorFunction: arrowView.anchorFunction,
                                      arrowViews: this.views});

        this.destroy();
    },

    selected: function(selected) {
        if (selectionManager.size() !== 1) {
            this.destroy();
        }
    },

    deselected: function(deselected) {
        if ((deselected.length === 1) &&
            (deselected[0] === this.attributes.geomNode.id)) {
            
            this.destroy();
        }
    },

    destroy: function(event) {
        selectionManager.off('deselected', this.deselected);
        this.views.map(function(view) {
            view.remove();
        });
    },

});

SS.SceneObjectView = Backbone.View.extend({
    
    initialize: function() {
        this.sceneObject = new THREE.Object3D(); 
	SS.sceneView.registerSceneObjectView(this);
        
        this.render();
        this.postRender();
    },

    clear: function() {
        var children = this.sceneObject.children;
        var that = this
        children.map(function(child) {
            that.sceneObject.remove(child);
        });
    },

    postRender: function() {
        SS.sceneView.scene.add(this.sceneObject);
    },

    remove: function() {
	SS.sceneView.deregisterSceneObjectView(this);
        SS.sceneView.scene.remove(this.sceneObject);
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

SS.ScaleTransformArrowView = SS.SceneObjectView.extend({
    
    initialize: function() {
	SS.SceneObjectView.prototype.initialize.call(this);
	this.on('mouseEnter', this.highlight);
	this.on('mouseLeave', this.unhighlight);

        this.on('mouseDown', function(event) {
            this.model.mouseDownOnArrow && this.model.mouseDownOnArrow(this, event);
        }, this);
        this.model.on("change", this.render, this);
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

    highlight: function() {
	SS.recursiveHighlightFn(this.sceneObject, 1.0);
    },

    unhighlight: function() {
	SS.recursiveHighlightFn(this.sceneObject, 0.5);
    }
    
});

SS.ScaleTransformArrowViewMaxXMaxY = SS.ScaleTransformArrowView.extend({
    
    initialize: function() {
	SS.ScaleTransformArrowView.prototype.initialize.call(this);
        this.anchorFunction = function(boundingBox) {
            return {x: boundingBox.max.x, 
                    y: boundingBox.max.y};
        }
    },

    render: function() {
        SS.ScaleTransformArrowView.prototype.render.call(this);
        this.sceneObject.position.x = this.model.boundingBox.max.x + 1;
        this.sceneObject.position.y = this.model.boundingBox.max.y + 1;
        this.sceneObject.rotation.z = 1/4*Math.PI;
        this.postRender();
        return this;
    }
});

SS.ScaleTransformArrowViewMinXMaxY = SS.ScaleTransformArrowView.extend({

    initialize: function() {
	SS.ScaleTransformArrowView.prototype.initialize.call(this);
        this.anchorFunction = function(boundingBox) {
            return {x: boundingBox.min.x, 
                    y: boundingBox.max.y};
        }
    },

    render: function() {
        SS.ScaleTransformArrowView.prototype.render.call(this);
        this.sceneObject.position.x = this.model.boundingBox.min.x - 1;
        this.sceneObject.position.y = this.model.boundingBox.max.y + 1;
        this.sceneObject.rotation.z = 3/4*Math.PI;
        this.postRender();
        return this;
    }
});

SS.ScaleTransformArrowViewMinXMinY = SS.ScaleTransformArrowView.extend({

    initialize: function() {
	SS.ScaleTransformArrowView.prototype.initialize.call(this);
        this.anchorFunction = function(boundingBox) {
            return {x: boundingBox.min.x, 
                    y: boundingBox.min.y};
        }
    },

    render: function() {
        SS.ScaleTransformArrowView.prototype.render.call(this);
        this.sceneObject.position.x = this.model.boundingBox.min.x - 1;
        this.sceneObject.position.y = this.model.boundingBox.min.y - 1;
        this.sceneObject.rotation.z = 5/4*Math.PI;
        this.postRender();
        return this;
    }
});

SS.ScaleTransformArrowViewMaxXMinY = SS.ScaleTransformArrowView.extend({

    initialize: function() {
	SS.ScaleTransformArrowView.prototype.initialize.call(this);
        this.anchorFunction = function(boundingBox) {
            return {x: boundingBox.max.x, 
                    y: boundingBox.min.y};
        }
    },

    render: function() {
        SS.ScaleTransformArrowView.prototype.render.call(this);
        this.sceneObject.position.x = this.model.boundingBox.max.x + 1;
        this.sceneObject.position.y = this.model.boundingBox.min.y - 1;
        this.sceneObject.rotation.z = 7/4*Math.PI;
        this.postRender();
        return this;
    }
});


