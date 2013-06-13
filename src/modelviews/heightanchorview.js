define([
    'calculations',
    'scene',
    'geometrygraphsingleton',
    'settings',
    'modelviews/workplaneMV',
    'modelviews/geomvertexMV',
    ], function(calc, sceneModel, geometryGraph, settings, WorkplaneMV, GeomVertexMV) {

    var EditingHeightAnchor = GeomVertexMV.EditingSceneView.extend({

        initialize: function(options) {
            this.pointVertex = options.pointVertex;
            this.heightKey = options.heightKey;
            GeomVertexMV.EditingSceneView.prototype.initialize.call(this);
            this.render();
            this.on('dragStarted', this.dragStarted, this);
            this.on('drag', this.drag, this);
            this.on('dragEnded', this.dragEnded, this);
        },

        remove: function() {
            GeomVertexMV.EditingSceneView.prototype.remove.call(this);
            this.off('dragStarted', this.dragStarted, this);
            this.off('drag', this.drag, this);
            this.off('dragEnded', this.dragEnded, this);
        },

        render: function() {
            GeomVertexMV.EditingSceneView.prototype.render.call(this);
            var ambient = this.highlightAmbient || this.selectedAmbient || this.ambient || 0x333333;
            var color = this.highlightColor || this.selectedColor || this.color || 0x00dd00;

            this.pointSceneObject = THREE.SceneUtils.createMultiMaterialObject(
                new THREE.CylinderGeometry(0, 0.75, 1.5, 3), 
                [
                    new THREE.MeshBasicMaterial({color: 0x993333, opacity: 0.5, wireframe: false } ),
                    new THREE.MeshBasicMaterial({color: 0xcc6666, wireframe: true})
                ]);
            var pointPosition = calc.objToVector(this.pointVertex.parameters.coordinate, geometryGraph, THREE.Vector3);
            var heightParameterValue = geometryGraph.evaluate(this.model.vertex.parameters[this.heightKey]);
            var zOffset = this.getZOffset(heightParameterValue);
            this.pointSceneObject.position = pointPosition;
            this.pointSceneObject.position.z = 
                heightParameterValue + 
                pointPosition.z + zOffset;
            this.pointSceneObject.scale = this.cameraScale;
            this.pointSceneObject.rotation.x = heightParameterValue >= 0 ? Math.PI/2 : 3*Math.PI/2;
            this.sceneObject.add(this.pointSceneObject);

            if (this.showHeightLine) {
                var lineGeometry = new THREE.Geometry();
                lineGeometry.vertices.push(pointPosition.clone().setZ(-1000));
                lineGeometry.vertices.push(pointPosition.clone().setZ(1000));
                var line = new THREE.Line(lineGeometry, new THREE.LineBasicMaterial({color: 0xff6666}));
                this.sceneObject.add(line);
            }

        },

        updateScaledObjects: function() {
            this.pointSceneObject.scale = this.cameraScale;
        },

        getZOffset: function(heightParameterValue) {
            return heightParameterValue < 0 ? -1.5*this.cameraScale.x : 1.5*this.cameraScale.x;
        },

        isDraggable: function() {
            return true;
        },

        dragStarted: function() {
            this.showHeightLine = true;
        },

        dragEnded: function() {
            this.showHeightLine = false;
            this.model.vertex.trigger('change', this.model.vertex);
        },

        drag: function(workplanePosition, intersection, event) {
            this.dragging = true;

            var sceneElement = $('#scene');
            var camera = sceneModel.view.camera;
            var mouseRay = calc.mouseRayForEvent(sceneElement, camera, event);

            var rayOrigin = calc.objToVector(this.pointVertex.parameters.coordinate, geometryGraph, THREE.Vector3);
            var rayDirection = new THREE.Vector3(0,0,1);
            var ray = new THREE.Ray(rayOrigin, rayDirection);

            var positionOnNormal = calc.positionOnRay(mouseRay, ray);
            var pointPosition = calc.objToVector(this.pointVertex.parameters.coordinate, geometryGraph, THREE.Vector3);
            var grid = settings.get('gridsize');
            this.model.vertex.parameters[this.heightKey] = 
                Math.round(parseFloat((positionOnNormal.z -  this.cameraScale.x).toFixed(0))/grid)*grid - 
                pointPosition.z;

            this.model.vertex.trigger('change', this.model.vertex);
        },

    });
    
    return EditingHeightAnchor;

});
