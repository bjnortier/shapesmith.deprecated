var SS = SS || {};

SS.objToVector = function(obj) {
    return new THREE.Vector3(obj.x, obj.y, obj.z);
}


SS.rotateAroundAxis = function(position, axis, angle) {
    var quaternion = new THREE.Quaternion().setFromAxisAngle(axis, angle/180*Math.PI);
    var newPosition = quaternion.multiplyVector3(position, new THREE.Vector3());
    return newPosition;
}

SS.toScreenCoordinates = function(worldCoordinates) {
    var projScreenMat = new THREE.Matrix4();
    projScreenMat.multiply(SS.sceneView.camera.projectionMatrix, 
                           SS.sceneView.camera.matrixWorldInverse);
    projScreenMat.multiplyVector3(worldCoordinates);
    return {
        x: window.innerWidth * ((worldCoordinates.x+1)/2),
        y: window.innerHeight * ((-worldCoordinates.y+1)/2)
    }
}
