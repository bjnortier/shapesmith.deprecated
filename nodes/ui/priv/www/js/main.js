requirejs(
    [
        'src/interactioncoordinator',
        'src/geomtoolbar',
        'src/workplane',
        'src/trackball',
        'src/commandstack',
        'src/geometrygraphsingleton',
        'src/vertexmodelmanager',
        'src/webdriverutils',
    ], function(coordinator, geomToolbar, workplane, trackBall, commandStack, geometryGraph) {

    window.onpopstate = function(event) { 
        var commit = (event.state && event.state.commit) || $.getQueryParam("commit");
        if (!commandStack.pop(commit)) {
            geometryGraph.loadFromCommit(commit);
        }
    }

});

