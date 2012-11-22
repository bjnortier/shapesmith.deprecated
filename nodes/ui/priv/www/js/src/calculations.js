define([], function() {

    var copyObj = function(value) {
        if ((value === null) || (value === undefined)) {
            return undefined;
        } if (Object.prototype.toString.call(value) === '[object Array]') {
            return value.map(function(x) {
                return copyObj(x);
            });
        } else if (typeof(value) === 'object') {
            var returnObj = {};
            for (var key in value) {
                if (value.hasOwnProperty(key)) {
                    returnObj[key] = copyObj(value[key]);
                }
            }
            return returnObj;
        } else {
            return value;
        }
    }
    
    var positionOnWorkplane = function(event, workplaneNode, camera) {

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

        var vector = new THREE.Vector3(mouse.x, mouse.y, 0.5);
        var projector = new THREE.Projector();
        var mouse3D = projector.unprojectVector(vector, camera);

        var ray = new THREE.Ray(camera.position); // To 0,0,0 is the default    
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

    var positionOnRay = function(event, ray, camera) {
        var mouse = {};
        mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
        mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;
            
        var vector = new THREE.Vector3( mouse.x, mouse.y, 0.5 );
        var projector = new THREE.Projector();
        var mouse3D = projector.unprojectVector(vector, camera);
        var mouseRay = new THREE.Ray(camera.position, null);
        mouseRay.direction = mouse3D.subSelf(camera.position).normalize();

        mouseRay.origin = positionOnPlane(event, new THREE.Vector3(0,0,0), new THREE.Vector3(0,0,1), camera);
        if (mouseRay.origin === undefined) {
            return;
        }

        // http://softsurfer.com/Archive/algorithm_0106/algorithm_0106.htm
        var u = ray.direction.clone().normalize();
        var v = mouseRay.direction.clone().normalize();

        var w0 = new THREE.Vector3().sub(ray.origin.clone(), mouseRay.origin.clone());

        var a = u.dot(u), b = u.dot(v), c = v.dot(v), d = u.dot(w0), e = v.dot(w0);
        
        var sc = (b*e - c*d)/(a*c - b*b);
        var tc = (a*e - b*d)/(a*c - b*b);
        
        return  new THREE.Vector3().add(ray.origin, u.clone().multiplyScalar(sc));
    }

    var toScreenCoordinates = function(camera, worldCoordinates) {
        var projScreenMat = new THREE.Matrix4();
        projScreenMat.multiply(camera.projectionMatrix, 
                               camera.matrixWorldInverse);
        projScreenMat.multiplyVector3(worldCoordinates);
        return {
            x: window.innerWidth * ((worldCoordinates.x+1)/2),
            y: window.innerHeight * ((-worldCoordinates.y+1)/2)
        }
    }

    var objToVector = function(obj, geometryGraph) {
        var eval = function(expression) {
            if (geometryGraph) {
                return geometryGraph.evaluate(expression);
            } else {
                return expression;
            }
        }
        if (obj.hasOwnProperty('x')) {
            return new THREE.Vector3(
                eval(obj.x), 
                eval(obj.y), 
                eval(obj.z));
        } else {
            return new THREE.Vector3(
                eval(obj.u), 
                eval(obj.v), 
                eval(obj.w));
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
        copyObj             : copyObj,
        positionOnWorkplane : positionOnWorkplane,
        positionOnPlane     : positionOnPlane,
        positionOnRay       : positionOnRay,
        toScreenCoordinates : toScreenCoordinates,
        objToVector         : objToVector,
    }

});