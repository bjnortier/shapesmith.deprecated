var SS = SS || {};

(function() {
    
    var selectInScene = function(objects, camera, event, filterFn) {
        
        var mouse = {};
        mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
        mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

        var vector = new THREE.Vector3( mouse.x, mouse.y, 0.5);
        var projector = new THREE.Projector();
        var mouse3D = projector.unprojectVector(vector, camera);
        var ray = new THREE.Ray(camera.position, null);
        ray.direction = mouse3D.subSelf(camera.position).normalize();

        // Only intersect mesh objects
        var allMeshes = [];
        var searchFn = function(obj) {
            if (obj.constructor == THREE.Mesh) {
                allMeshes.push(obj);
            }
            if (obj.children && (obj.children.length > 0)) {
                obj.children.map(searchFn);
            }
        }
        var searchableChildren = objects.filter(filterFn)
        searchFn({children: searchableChildren});
        var intersects = ray.intersectObjects(allMeshes);

        return intersects;

    }
    
    SS.selectGeomNodesInScene = function(objects, camera, event) {
        var filterFn = function(child) {
            return child.name.geomNodeId !== undefined;
        }
        return selectInScene(objects, camera, event, filterFn);
    }

    SS.selectNonGeomNodesInScene = function(objects, camera, event) {
        var filterFn = function(child) {
            return (child.name.geomNodeId === undefined)  && (child.constructor !== THREE.Line);
        }
        return selectInScene(objects, camera, event, filterFn);
    }

})();
