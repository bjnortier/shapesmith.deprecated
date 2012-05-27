var SS = SS || {};

(function() {
    
    var selectInScene = function(scene, camera, event, filterFn) {
        
        var mouse = {};
        mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
        mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

        var vector = new THREE.Vector3( mouse.x, mouse.y, 0.5 );
        var projector = new THREE.Projector();
        var mouse3D = projector.unprojectVector(vector, camera);
        var ray = new THREE.Ray(camera.position, null);
        ray.direction = mouse3D.subSelf(camera.position).normalize();

        // Only intersect mesh objects
        var meshObjects = scene.children.filter(filterFn);
        var intersects = ray.intersectObjects(meshObjects);

        return intersects;

    }
    
    SS.selectGeomNodesInScene = function(scene, camera, event) {
        var filterFn = function(child) {
            return child.name.geomNodeId !== undefined;
        }
        return selectInScene(scene, camera, event, filterFn);
    }

    SS.selectNonGeomNodesInScene = function(scene, camera, event) {
        var filterFn = function(child) {
            return child.name.geomNodeId === undefined;
        }
        return selectInScene(scene, camera, event, filterFn);
    }

})();
