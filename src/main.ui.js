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
        'latheapi/adapter'
    ], function(adapter) {

        // Initialize the BSP DB before anything else
        adapter.broker.on('initialized', function() {

            requirejs(['designer'], function(designer) {

                requirejs([
                    'jquery',
                    'lib/jquery.getQueryParam',
                    'scene',
                    'interactioncoordinator',
                    'worldcursor',
                    'trackball',
                    'commandstack',
                    'geometrygraphsingleton',
                    'casgraph/ajaxreplicator',
                    'variablemanager',       
                    'modelviews/modelgraph',
                    'modelviews/objecttree',
                    'webdriverutils',
                    'asyncAPI',
                    'hintview',
                    'inspect/statsview',
                    'scripting/designer',
                ], function(
                    $, _$,
                    sceneModel,
                    coordinator,
                    worldCursor,
                    trackBall,
                    commandStack,
                    geometryGraph,
                    AJAXReplicator,
                    variableManager,
                    modelGraph,
                    objectTree,
                    wdutils,
                    AsyncAPI,
                    hintView,
                    StatsView,
                    Designer) {

                $(document).ready(function() {

                    designer.init();

                    var statsView = new StatsView();

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

                    window.designer = new Designer();

                });

            });

        });

    });

});

