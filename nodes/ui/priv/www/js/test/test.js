var assert = require('chai').assert,
    _und = require('underscore'),
    geomNode = require('../src/geomnode');

describe('Point', function() {

    it('constructs correctly', function() {
        var point = new geomNode.Point();

        assert.equal(point.type, 'point');
        assert.equal(point.id.indexOf('point'), 0);
        assert.isUndefined(point.name);
        assert.isFalse(point.editing);
        assert.deepEqual(point.parameters, {coordinate: {x:0, y:0, z:0}});
    });

    it('has mutable parameters', function() {

        var point = new geomNode.Point({parameters: {coordinate: {
            x: 1, y: 2, z:3,
        }}});

        point.parameters.coordinate.x = 10;

        assert.deepEqual(point.parameters, {coordinate: {x:10, y:2, z:3}});

    });

    it('clones nonEditing correctly', function() {

        var point = new geomNode.Point({parameters: {coordinate: {
            x: 1, y: 2, z:3,
        }}});

        var nonEditing = point.cloneNonEditing();

        assert.notEqual(point.id, nonEditing.id);
        assert.equal(point.name, nonEditing.name);
        assert.deepEqual(point.parameters, nonEditing.parameters);
        assert.isFalse(nonEditing.editing);

    });

});

describe('Polyline', function() {

    if('constructs properly') {
        var polyline = new geomNode.Polyline();
    }

});

