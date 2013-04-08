define([
        'backbone',
        'selection',
        'scene',
        'scenevieweventgenerator',
        'geometrygraphsingleton',
        'asyncAPI',
    ], 
    function(
        Backbone, 
        selection, 
        sceneModel,
        sceneViewEventGenerator,
        geometryGraph,
        AsyncAPI) {

    var Model = Backbone.Model.extend({

        initialize: function() {
            // selection.on('selected', this.selected, this);
            // selection.on('deselected', this.deselected, this);
        },

        deselected: function() {
            if (this.sceneView) {
                this.sceneView.remove();
                delete this.sceneView;
            }
        },

        selected: function(_, selected) {
            if (selected.length === 1) {
                this.vertex = geometryGraph.vertexById(selected[0]);
                this.sceneView = new SceneView({model: this});
            } else {
                if (this.sceneView) {
                    this.sceneView.remove();
                    delete this.sceneView;
                }
            }
        },

    })

    var SceneView = Backbone.View.extend({

        initialize: function() {
            this.scene = sceneModel.view.scene;
            this.updateCameraScale();
            this.initialPosition = new THREE.Vector3();
            this.render();
            sceneModel.view.on('cameraMoved', this.cameraMoved, this);
            sceneModel.view.on('cameraMoveStopped', this.cameraMoveStopped, this);
            this.on('dragStarted', this.dragStarted, this);
            this.on('dragEnded', this.dragEnded, this);
            this.on('drag', this.drag, this);
        },

        remove: function() {
            this.scene.remove(this.sceneObject);
            sceneViewEventGenerator.deregister(this);
            sceneModel.view.updateScene = true;
            sceneModel.view.off('cameraMoved', this.cameraMoved, this);
            sceneModel.view.off('cameraMoveStopped', this.cameraMoveStopped, this);
            this.off('dragStarted', this.dragStarted, this);
            this.off('dragEnded', this.dragEnded, this);
            this.off('drag', this.drag, this);
        },

        render: function() {
            this.clear();

            var arrowGeometry = new THREE.Geometry();
            var a = 8, b = 5, c = 2, d = 1;
            var positions = [
                [a, 0, 0],
                [b, c, 0],
                [b, d, 0],
                [d, d, 0],
                [d, b, 0],
                [c, b, 0],
                [0, a, 0],
                [-c, b, 0],
                [-d, b, 0],
                [-d, d, 0],
                [-b, d, 0],
                [-b, c, 0],
                [-a, 0, 0],
                [-b, -c, 0],
                [-b, -d, 0],
                [-d, -d, 0],
                [-d, -b, 0],
                [-c, -b, 0],
                [0, -a, 0],
                [c, -b, 0],
                [d, -b, 0],
                [d, -d, 0],
                [b, -d, 0],
                [b, -c, 0],
                [a, 0, 0],
            ];

            arrowGeometry.vertices = positions.map(function(pos) {
                return new THREE.Vector3(pos[0], pos[1], pos[2]);
            });
            arrowGeometry.faces.push(new THREE.Face3(23,0,1));
            arrowGeometry.faces.push(new THREE.Face3(5,6,7));
            arrowGeometry.faces.push(new THREE.Face3(11,12,13));
            arrowGeometry.faces.push(new THREE.Face3(17,18,19));
            arrowGeometry.faces.push(new THREE.Face4(21,3,9,15));
            arrowGeometry.faces.push(new THREE.Face4(21,22,2,3));
            arrowGeometry.faces.push(new THREE.Face4(3,4,8,9));
            arrowGeometry.faces.push(new THREE.Face4(9,10,14,15));
            arrowGeometry.faces.push(new THREE.Face4(15,16,20,21));
            arrowGeometry.computeCentroids();
            arrowGeometry.computeFaceNormals();
            
            var arrowMesh = new THREE.Mesh(
                arrowGeometry, 
                new THREE.MeshBasicMaterial({
                    color: 0x0099cc, 
                    side: THREE.DoubleSide,
                }));
            
            var lineGeom = new THREE.Geometry();
            lineGeom.vertices = arrowGeometry.vertices;
            var line = new THREE.Line(
                lineGeom, 
                new THREE.LineBasicMaterial({
                    color: 0x007088, 
                    wireframe : true, 
                    linewidth: 2.0, 
                }));

            this.arrowObject = new THREE.Object3D();
            this.arrowObject.scale = this.cameraScale;
            this.arrowObject.position = this.initialPosition;
            this.arrowObject.add(arrowMesh);
            this.arrowObject.add(line);
            this.sceneObject.add(this.arrowObject);
        },

        clear: function() {
            if (this.sceneObject) {
                this.scene.remove(this.sceneObject);
                sceneViewEventGenerator.deregister(this);
            }

            this.sceneObject = new THREE.Object3D();
            this.hiddenSelectionObject = new THREE.Object3D();
            this.scene.add(this.sceneObject);
            sceneViewEventGenerator.register(this);
            sceneModel.view.updateScene = true;
        },

        isClickable: function() {
            return false;
        },

        isDraggable: function() {
            return true;
        },

        cameraMoved: function() {
            this.updateCameraScale();
            sceneModel.view.updateScene = true;
        },

        updateCameraScale: function() {
            var camera = sceneModel.view.camera;
            var cameraDistance = camera.position.length();
            var newScale = cameraDistance/200;
            this.cameraScale = new THREE.Vector3(newScale, newScale, newScale);
            this.arrowObject && (this.arrowObject.scale = this.cameraScale);
        },

        dragStarted: function() {
            this.originalVertex = this.model.vertex;
            this.originalVertex.transforming = true;
            this.editingVertex = AsyncAPI.edit(this.model.vertex);
            this.transform = {
                translate: {
                    x: 0, y:0, z: 0
                }
            }
            this.editingVertex.transforms.push(this.transform);
        },

        drag: function(position) {
            this.arrowObject.position = new THREE.Vector3().addVectors(this.initialPosition, position);
            sceneModel.view.updateScene = true;

            this.transform.translate.x = this.arrowObject.position.x;
            this.transform.translate.y = this.arrowObject.position.y;
            this.transform.translate.z = this.arrowObject.position.z;
            this.editingVertex.trigger('change', this.editingVertex);

        },

        dragEnded: function() {
            this.editingVertex.transforming = false;
            AsyncAPI.tryCommitEdit([this.originalVertex], [this.editingVertex]);
        },


    });

    return new Model();


});