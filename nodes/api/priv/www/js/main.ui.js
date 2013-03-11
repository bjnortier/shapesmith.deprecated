requirejs.config({
    paths: {
        'underscore': 'node_modules/underscore/underscore',
        'backbone-events': 'node_modules/backbone-events/lib/backbone-events',
        'backbone': 'node_modules/backbone/backbone',
        'lathe': 'node_modules/lathe/lib',
    },
    shim: {
        'underscore': {
            exports: '_'
        },
        'backbone': {
            deps: ['underscore', 'jquery'],
            exports: 'Backbone'
        },
    },
});

requirejs([
        'jquery',
        'lib/jquery.getQueryParam',
        'src/scene',
        'src/interactioncoordinator',
        'src/worldcursor',
        'src/geomtoolbar',
        'src/workplaneMV',
        'src/trackball',
        'src/commandstack',
        'src/geometrygraphsingleton',
        'src/casgraph/ajaxreplicator',
        'src/variablemanager',       
        'src/vertexmodelmanager',
        'src/webdriverutils',
        'src/maintoolbar',
        'src/asyncAPI',
        'src/geomnode',
        'src/hintview',
        'src/inspect/statsview',
        'src/inspect/renderingoptionsview',
        'src/splashscreen',
        'src/scripting/designer',
        'src/layers/layertreemanager',       
    ], function(
        $, _$,
        sceneModel,
        coordinator,
        worldCursor,
        geomToolbar,
        Workplane,
        trackBall,
        commandStack,
        geometryGraph,
        AJAXReplicator,
        variableManager,
        vertexModelManager,
        wdutils,
        mainToolbar,
        AsyncAPI,
        geomNode,
        hintView,
        StatsView,
        RenderingOptionsView,
        SplashScreen,
        Designer,
        layertreeManager) {

    var originalReplaceFn = Workplane.DisplayModel.prototype.vertexReplaced;

    $(document).ready(function() {
        var resizeContainers = function() {
            sceneModel.view.resize();
            $('#variables').css('width', $('#explorer').width() + 'px')
            $('#geometry').css('width', $('#explorer').width() + 'px')
        }

        var statsView = new StatsView();
        // var renderingOptionsView = new RenderingOptionsView();

        var vertexUrl = '/_api/' + globals.user + '/' + globals.design + '/vertex/';
        var graphUrl = '/_api/' + globals.user + '/' + globals.design + '/graph/';
        var replicator = new AJAXReplicator(vertexUrl, graphUrl);
        geometryGraph.attachReplicator(replicator);

        var commitSHA = $.getQueryParam("commit");
        AsyncAPI.loadFromCommit(replicator, commitSHA, function() {

            worldCursor.registerEvents();
            window.onpopstate = function(event) { 
            
                var commit = (event.state && event.state.commit) || $.getQueryParam("commit");
                if (!commandStack.pop(commit)) {
                    AsyncAPI.loadFromCommit(replicator, commit);
                }    
            }
        });

        // if ($.getQueryParam('splash')) {
        //     new SplashScreen();
        // }

        window.designer = new Designer();

    });

});

