var SS = SS || {};

define(['src/scene', 'src/trackball', 'src/calculations'], 
    function(sceneModel, trackball, calc) {

    SS.toWorldPos = function(x, y) {

    }

    SS.dontDampTrackball = function() {
        trackball.dontDamp();
    }

    SS.toScreenCoordinates = function(x, y, z) {
        var camera = sceneModel.view.camera;
        return calc.toScreenCoordinates(camera, new THREE.Vector3(x,y,z));
    }

    SS.zoomIn = function() {
        trackball.zoom(100);
        trackball.updateCamera();
    }

});