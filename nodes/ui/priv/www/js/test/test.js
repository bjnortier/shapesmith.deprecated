var assert = require('chai').assert,
    requirejs = require('requirejs');

requirejs.config({
    //Pass the top-level main.js/index.js require
    //function to requirejs so that node modules
    //are loaded relative to the top-level JS file.
    baseUrl: '..',
    nodeRequire: require
});

var geomNode = requirejs('src/geomnode');
var geometryGraph = requirejs('src/geometrygraph');

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

    it('constructs properly', function() {
        var polyline = new geomNode.Polyline();
        assert.deepEqual(polyline.parameters, {coordinates: [{x:0, y:0, z:0}]});
    });

});

describe('GeometryGraph', function() {

    it('can be empty', function() {
        var graph = new geometryGraph.Graph();

        assert.equal(graph.vertexCount(), 0);
    });

    it('manages incoming and outoing vertices', function() {

        var a = {id:'a'}, b = {id:'b'}, c = {id:'c'};
        var graph = new geometryGraph.Graph();
        assert.equal(graph.vertexCount(), 0);

        graph.addVertex(a);        
        graph.addVertex(b); 
        graph.addVertex(c); 
        graph.addEdge(a,b);
        graph.addEdge(a,c);
        graph.addEdge(b,c);

        assert.equal(graph.vertexCount(), 3);
        assert.deepEqual(graph.outgoingEdgesOf(a), ['b', 'c']);
        assert.deepEqual(graph.outgoingEdgesOf(b), ['c']);
        assert.deepEqual(graph.outgoingEdgesOf(c), []);
        assert.deepEqual(graph.incomingEdgesOf(a), []);
        assert.deepEqual(graph.incomingEdgesOf(b), ['a']);
        assert.deepEqual(graph.incomingEdgesOf(c), ['a', 'b']);

        graph.removeVertex(b);

        assert.equal(graph.vertexCount(), 2);
        assert.deepEqual(graph.outgoingEdgesOf(a), ['c']);
        assert.deepEqual(graph.outgoingEdgesOf(b), []);
        assert.deepEqual(graph.outgoingEdgesOf(c), []);
        assert.deepEqual(graph.incomingEdgesOf(a), []);
        assert.deepEqual(graph.incomingEdgesOf(b), []);
        assert.deepEqual(graph.incomingEdgesOf(c), ['a']);

        graph.removeVertex(a);

        assert.equal(graph.vertexCount(), 1);
        assert.deepEqual(graph.outgoingEdgesOf(a), []);
        assert.deepEqual(graph.outgoingEdgesOf(b), []);
        assert.deepEqual(graph.outgoingEdgesOf(c), []);
        assert.deepEqual(graph.incomingEdgesOf(a), []);
        assert.deepEqual(graph.incomingEdgesOf(b), []);
        assert.deepEqual(graph.incomingEdgesOf(c), []);

    });

    it('support a graph with one Point vertex', function() {
        var graph = new geometryGraph.Graph();

        var point = graph.createPointPrototype();

        assert.equal(graph.vertexCount(), 1);
        assert.equal(graph.outgoingEdgesOf(point).length, 0);        
    });

    it('can support a polyline with point children', function() {
        var graph = new geometryGraph.Graph();
        var polyline = graph.createPolylinePrototype();
        assert.equal(graph.outgoingEdgesOf(polyline).length, 1); 

        assert.equal(graph.vertexCount(), 2);
        assert.equal(graph.outgoingEdgesOf(polyline).length, 1); 

        graph.addPointToPolyline(polyline);

        assert.equal(graph.vertexCount(), 3);
        assert.equal(graph.outgoingEdgesOf(polyline).length, 2); 

    });
})



