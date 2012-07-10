var SS = SS || {};

SS.objToVector = function(obj) {
    if (obj.hasOwnProperty('x')) {
        return new THREE.Vector3(obj.x, obj.y, obj.z);
    } else {
        return new THREE.Vector3(obj.u, obj.v, obj.w);
    }
}

SS.rotateAroundAxis = function(position, axis, angle) {
    if (angle !== 0) {
        var quaternion = new THREE.Quaternion().setFromAxisAngle(axis, angle/180*Math.PI);
        var newPosition = quaternion.multiplyVector3(position, new THREE.Vector3());
        return newPosition;
    } else {
        return position.clone();
    }
}

SS.worldPositionFromWorkplanePosition = function(position, workplaneNode) {
    return new THREE.Vector3().add(
        SS.rotateAroundAxis(position, SS.objToVector(workplaneNode.axis), workplaneNode.angle),
        SS.objToVector(workplaneNode.origin));
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

SS.boundingBoxToScreenPosition = function(boundingBox) {

    var projScreenMat = new THREE.Matrix4();
    projScreenMat.multiply(SS.sceneView.camera.projectionMatrix, 
                           SS.sceneView.camera.matrixWorldInverse);

    var xminmax = [boundingBox.min.x, boundingBox.max.x];
    var yminmax = [boundingBox.min.y, boundingBox.max.y];
    var zminmax = [boundingBox.min.z, boundingBox.max.z];
    var screenMaxX = undefined, screenMinY = undefined, screenMaxY = undefined;

    for (var i = 0; i < 2; i++) {
        for (var j = 0; j < 2; ++j) {
            for (var k = 0; k < 2; ++k) {
                var worldPos = new THREE.Vector3(xminmax[i], yminmax[j], zminmax[k]);
                var screenPos = SS.toScreenCoordinates(worldPos);
                screenMaxX = screenMaxX ? Math.max(screenPos.x, screenMaxX) : screenPos.x;
                screenMinY = screenMinY ? Math.min(screenPos.y, screenMinY) : screenPos.y;
                screenMaxY = screenMaxY ? Math.max(screenPos.y, screenMaxY) : screenPos.y;
            }
        }
    }

    var boundary = 100;
    var xPos = screenMaxX;
    var yPos = (screenMaxY + screenMinY)/2;
    xPos = Math.max(xPos, boundary);
    yPos = Math.max(yPos, boundary);
    xPos = Math.min(xPos, window.innerWidth - boundary - 50);
    yPos = Math.min(yPos, window.innerHeight - boundary - 50);
    return {x: xPos, y:yPos};

}

SS.copyObj = function(value) {
    if ((value === null) || (value === undefined)) {
        return undefined;
    } if (Object.prototype.toString.call( value ) === '[object Array]') {
        return value.map(function(x) {
            return SS.copyObj(x);
        });
    } else if (typeof(value) === 'object') {
        var returnObj = {};
        for (var key in value) {
            returnObj[key] = SS.copyObj(value[key]);
        }
        return returnObj;
    } else {
        return value;
    }
}
