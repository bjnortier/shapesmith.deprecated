requirejs(
    [
        'src/interactioncoordinator',
        'src/geomtoolbar',
        'src/workplane',
        'src/trackball',
        'src/commandstack',
        'src/geometrygraphsingleton',
        'src/variablemanager',
        'src/vertexmodelmanager',
        'src/webdriverutils',
        'src/maintoolbar',
    ], function(
        coordinator,
        geomToolbar,
        workplane,
        trackBall,
        commandStack,
        geometryGraph,
        variableManager,
        vertexModelManager,
        wdutils,
        mainToolbar) {

    // *Sometimes* chrome will not generate a postate event on initial load
    // and Firefox doesn't popstate() on initial load. So for initial load we 
    // use document.ready() and bind the popstate function afterwards

    setTimeout(function() {
        window.onpopstate = function(event) { 
            var commit = (event.state && event.state.commit) || $.getQueryParam("commit");
            if (!commandStack.pop(commit)) {
                geometryGraph.loadFromCommit(commit);
            }
        }
    }, 500);

    $(document).ready(function() {
        geometryGraph.loadFromCommit($.getQueryParam("commit"));   
    });

});

