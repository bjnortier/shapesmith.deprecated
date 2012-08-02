var SS = SS || {};


SS.DimensionArrowsView = SS.InteractiveSceneView.extend({

    priority: -1,    
    active: false,

    createDimArrow: function(label, vector) {
        var scale = this.cameraScale;

        var object = new THREE.Object3D();

        var arrowGeom = new THREE.Geometry();
        arrowGeom.vertices.push(new THREE.Vector3(0,0,0));
        arrowGeom.vertices.push(new THREE.Vector3(0,0,vector.clone().length()));
        var line = new THREE.Line(arrowGeom, SS.materials.lineMaterial);

        var pointGeom = new THREE.CylinderGeometry(0, 0.5*scale, 1.5*scale, 3);
        var materials = [
            SS.materials.faceMaterial,
            SS.materials.wireframeMaterial,
        ];
        var point = THREE.SceneUtils.createMultiMaterialObject(pointGeom, materials);

        point.position.z = vector.clone().length() - 0.75*scale;
        point.rotation.x = Math.PI/2;
        
        object.add(line);
        object.add(point);
        object.lookAt(vector.clone().normalize());
        return object;
    }
});