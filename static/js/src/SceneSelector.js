var SS = SS || {};

SS.selectInScene = function(scene, camera, event) {

    var mouse = {};
    mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
    mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

    var vector = new THREE.Vector3( mouse.x, mouse.y, 0.5 );
    var projector = new THREE.Projector();
    var mouse3D = projector.unprojectVector(vector, camera);
    var ray = new THREE.Ray(camera.position, null);
    ray.direction = mouse3D.subSelf(camera.position).normalize();
    var intersects = ray.intersectScene(scene);

    return intersects;
}