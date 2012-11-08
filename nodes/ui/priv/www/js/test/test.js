var chai = require('chai'),
    assert = chai.assert,
    requirejs = require('requirejs');

chai.Assertion.includeStack = true;

requirejs.config({
    //Pass the top-level main.js/index.js require
    //function to requirejs so that node modules
    //are loaded relative to the top-level JS file.
    baseUrl: '..',
    nodeRequire: require
});

var geomNode = requirejs('src/geomnode');
var graphLib = requirejs('src/graph');

describe('Point', function() {

    it('constructs correctly', function() {
        var point = new geomNode.Point();

        assert.equal(point.type, 'point');
        assert.equal(point.id.indexOf('point'), 0);
        assert.isFalse(point.editing);
        assert.deepEqual(point.parameters, {coordinate: {x:0, y:0, z:0}});
    });

    it('can be named', function() {
        var point = new geomNode.Point();
        assert.equal(point.id, point.name);

        var cloned = point.cloneNonEditing();
        assert.equal(point.id, cloned.id);
        assert.equal(cloned.id, cloned.name);
    });

    it('has mutable parameters', function() {

        var point = new geomNode.Point({parameters: {coordinate: {
            x: 1, y: 2, z:3,
        }}});

        point.parameters.coordinate.x = 10;

        assert.deepEqual(point.parameters, {coordinate: {x:10, y:2, z:3}});

    });

    it('clones nonEditing correctly', function() {

        var point = new geomNode.Point({
            parameters: {
                coordinate: {
                    x: 1, y: 2, z:3,
                }
            }
        });

        var nonEditing = point.cloneNonEditing();

        assert.equal(point.id, nonEditing.id);
        assert.equal(point.name, nonEditing.name);
        assert.deepEqual(point.parameters, nonEditing.parameters);
        assert.isFalse(nonEditing.editing);

    });

});

describe('Polyline', function() {

    it('constructs properly', function() {
        var polyline = new geomNode.Polyline();
        assert.deepEqual(polyline.parameters, {coordinates: [{x:0, y:0, z:0}]});
    });

});





