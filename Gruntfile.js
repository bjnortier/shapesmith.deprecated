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

    express: {
        server: {
          options: {
            port: 8001,
            server: path.resolve('./src/api/server.js')
          }
        }
    }
  });

  grunt.loadNpmTasks('grunt-simple-mocha');

  // Unit testing
  grunt.registerTask('unit', ['simplemocha:unit']);
  
  // Functional testing - requires a running server
  grunt.loadNpmTasks('grunt-express');
  process.env['app_env'] = 'functional';
  grunt.registerTask('functional', ['express', 'simplemocha:functional']);

};
