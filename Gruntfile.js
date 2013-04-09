var path = require('path');

module.exports = function(grunt) {

  grunt.initConfig({
    simplemocha: {
      options: {
        // globals: ['should'],
        timeout: 3000,
        slow: 5000,
        ignoreLeaks: false,
        ui: 'bdd',
        reporter: 'spec',
        path: 'test'
      },

      unit: { 
        src: 'test/unit.js',
      },
      functional: { 
        src: [
            'test/functional/points.test.js',
            'test/functional/polylines.test.js',
        ],
      },
    },

    requirejs: {
      compile: {
        options: {
          appDir: ".",
          baseUrl: "src",
          dir: "build",
          optimize: "none",
          mainConfigFile: "src/main.ui.js",
          modules: [
            {
              name: "main.ui"
            }
          ]
        }
      }
    },

    express: {
        server: {
          options: {
            port: 8001,
            server: path.resolve('./src/api/server.js')
          }
        }
    },

    chmod: {
      options: {
        mode: '755'
      },
      build: {
        src: ['build/bin/start', 'build/node_modules/supervisor/lib/cli-wrapper.js']
      }
    }
  });

  grunt.loadNpmTasks('grunt-contrib-requirejs');
  grunt.loadNpmTasks('grunt-simple-mocha');
  grunt.loadNpmTasks('grunt-express');
  grunt.loadNpmTasks('grunt-chmod');

  // Unit testing
  grunt.registerTask('unit', ['simplemocha:unit']);
  
  // Functional testing - requires a running server
  process.env['app_env'] = 'functional';
  grunt.registerTask('functional', ['express', 'simplemocha:functional']);

  // Build the single JS file
  grunt.registerTask('build', ['requirejs', 'chmod:build'])

};
