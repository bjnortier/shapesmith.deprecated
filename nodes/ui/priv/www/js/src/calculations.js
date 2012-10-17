define([], function() {
    
    var posOnWorkplane = function(event, workplaneNode, camera) {

        var origin = objToVector(workplaneNode.origin);
        var axis   = objToVector(workplaneNode.axis);   
        var angle  = workplaneNode.angle;
        var normal = rotateAroundAxis(new THREE.Vector3(0,0,1), axis, angle);
        var worldPosition = positionOnPlane(event, origin, normal, camera);

        worldPosition.subSelf(origin);
        var workplanePosition = 
            rotateAroundAxis(worldPosition, axis, -workplaneNode.angle);

        return new THREE.Vector3(Math.round(workplanePosition.x),
                                 Math.round(workplanePosition.y),
                                 Math.round(workplanePosition.z));
    }

    function positionOnPlane(event, origin, normal, camera) {
        var mouse = {};
        mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
        mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

        var vector = new THREE.Vector3( mouse.x, mouse.y, 0.5);
        var projector = new THREE.Projector();
        var mouse3D = projector.unprojectVector(vector, camera);

        var ray = new THREE.Ray(camera.position, null);
        ray.direction = mouse3D.subSelf(camera.position).normalize();

        // http://en.wikipedia.org/wiki/Line-plane_intersection
        var p0 = origin;
        var l0 = ray.origin;
        var l = ray.direction;
        var n = normal;

        var d = new THREE.Vector3().sub(p0, l0).dot(n)/l.dot(n);
        if (d === 0) {
            return undefined;
        }
        return new THREE.Vector3().add(l0, l.clone().multiplyScalar(d));

    }

    var objToVector = function(obj) {
        if (obj.hasOwnProperty('x')) {
            return new THREE.Vector3(obj.x, obj.y, obj.z);
        } else {
            return new THREE.Vector3(obj.u, obj.v, obj.w);
        }
    }

    var rotateAroundAxis = function(position, axis, angle) {
        if (angle !== 0) {
            var quaternion = new THREE.Quaternion().setFromAxisAngle(axis, angle/180*Math.PI);
            var newPosition = quaternion.multiplyVector3(position, new THREE.Vector3());
            return newPosition;
        } else {
            return position.clone();
        }
    }

    return {
        posOnWorkplane : posOnWorkplane,
        objToVector    : objToVector,
    }

});