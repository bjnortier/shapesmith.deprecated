var SS = SS || {};

requirejs(
    [
        'src/interactioncoordinator',
        'src/scene', 
        'src/geomtoolbar',
        'src/workplane',
    ], function(
        interactionCoordinator,
        scene, 
        geomtoolbar,
        workplane
    ) {

    $(document).ready(function() {

        SS.interactionCoordinator = new interactionCoordinator.Coordinator();
        
        SS.sceneModel = new scene.Model();
        geomtoolbar.create();
        SS.workplaneModel = new workplane.Model({sceneView: SS.sceneModel.view});

    });

});