module.exports = function(grunt) {

  grunt.initConfig({
    simplemocha: {
      options: {
        // globals: ['should'],
        timeout: 3000,
        ignoreLeaks: false,
        ui: 'bdd',
        reporter: 'spec',
        path: 'test'
      },

      unit: { src: 'test/unit.js' },
      functional: { src: 'test/functional.js' },
    }
  });

  grunt.loadNpmTasks('grunt-simple-mocha');

  grunt.registerTask('default', ['simplemocha']);


};
