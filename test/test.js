var requirejs = require('requirejs');

requirejs.config({
    baseUrl: '..',
    nodeRequire: require,
});

var chai = requirejs('chai');
    assert = chai.assert;

chai.Assertion.includeStack = true;

var specs = requirejs('test/specs');
specs.forEach(function(spec) {
    requirejs(spec);
});
