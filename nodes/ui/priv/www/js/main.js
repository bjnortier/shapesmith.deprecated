requirejs(
    [
        'src/interactioncoordinator',
        'src/geomtoolbar',
        'src/workplaneMV',
        'src/trackball',
        'src/commandstack',
        'src/geometrygraphsingleton',
        'src/variablemanager',
        'src/vertexmodelmanager',
        'src/webdriverutils',
        'src/maintoolbar',
        'src/asyncAPI'
    ], function(
        coordinator,
        geomToolbar,
        Workplane,
        trackBall,
        commandStack,
        geometryGraph,
        variableManager,
        vertexModelManager,
        wdutils,
        mainToolbar,
        AsyncAPI) {

    // *Sometimes* chrome will not generate a postate event on initial load
    // and Firefox doesn't popstate() on initial load. So for initial load we 
    // use document.ready() and bind the popstate function afterwards

    setTimeout(function() {
        window.onpopstate = function(event) { 
            var commit = (event.state && event.state.commit) || $.getQueryParam("commit");
            if (!commandStack.pop(commit)) {
                AsyncAPI.loadFromCommit(commit);
            }
        }
    }, 500);

    $(document).ready(function() {
        AsyncAPI.loadFromCommit($.getQueryParam("commit"));   
    });

});

