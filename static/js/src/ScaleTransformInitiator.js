var SS = SS || {};

SS.ScaleTransformInitiator = Backbone.Model.extend({

    initialize: function() { 
        this.boundingBox = SS.boundingBoxForGeomNode(this.attributes.geomNode);
        this.position = new THREE.Vector3(0,0,0);
        new SS.ScaleTransformInitiatorViewMaxXMaxY({model: this}).render();
        new SS.ScaleTransformInitiatorViewMaxXMinY({model: this}).render();
        new SS.ScaleTransformInitiatorViewMinXMinY({model: this}).render();
        new SS.ScaleTransformInitiatorViewMinXMaxY({model: this}).render();
    },

});

SS.ScaleTransformInitiatorView = Backbone.View.extend({
    
    initialize: function() {
        this.sceneObject = new THREE.Object3D(); 
        this.model.bind('change', this.render, this);
    },

    render: function() {

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

        SS.sceneView.scene.add(this.sceneObject);
        return this;
    }
});

SS.ScaleTransformInitiatorViewMaxXMaxY = SS.ScaleTransformInitiatorView.extend({

    render: function() {
        SS.ScaleTransformInitiatorView.prototype.render.call(this);
        this.sceneObject.position.x = this.model.boundingBox.max.x + 1;
        this.sceneObject.position.y = this.model.boundingBox.max.y + 1;
        this.sceneObject.rotation.z = 1/4*Math.PI;
    }
});

SS.ScaleTransformInitiatorViewMinXMaxY = SS.ScaleTransformInitiatorView.extend({

    render: function() {
        SS.ScaleTransformInitiatorView.prototype.render.call(this);
        this.sceneObject.position.x = this.model.boundingBox.min.x - 1;
        this.sceneObject.position.y = this.model.boundingBox.max.y + 1;
        this.sceneObject.rotation.z = 3/4*Math.PI;
    }
});

SS.ScaleTransformInitiatorViewMinXMinY = SS.ScaleTransformInitiatorView.extend({

    render: function() {
        SS.ScaleTransformInitiatorView.prototype.render.call(this);
        this.sceneObject.position.x = this.model.boundingBox.min.x - 1;
        this.sceneObject.position.y = this.model.boundingBox.min.y - 1;
        this.sceneObject.rotation.z = 5/4*Math.PI;
    }
});

SS.ScaleTransformInitiatorViewMaxXMinY = SS.ScaleTransformInitiatorView.extend({

    render: function() {
        SS.ScaleTransformInitiatorView.prototype.render.call(this);
        this.sceneObject.position.x = this.model.boundingBox.max.x + 1;
        this.sceneObject.position.y = this.model.boundingBox.min.y - 1;
        this.sceneObject.rotation.z = 7/4*Math.PI;
    }
});


