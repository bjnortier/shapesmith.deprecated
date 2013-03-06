({
    appDir: ".",
    baseUrl: ".",
    dir: "build",
    optimize: "none",

    paths: {
        'jquery': 'empty:',
        'underscore': 'node_modules/underscore/underscore',
        'backbone-events': 'node_modules/backbone-events/lib/backbone-events',
        'backbone': 'node_modules/backbone/backbone'
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
    modules: [
        {
            name: "main.ui",
        }
    ]
})