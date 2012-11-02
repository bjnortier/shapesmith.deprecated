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

    it('can be named', function() {
        var point = new geomNode.Point({nameFromId: true});
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

describe('Graph', function() {

    it('can be empty', function() {
        var graph = new graphLib.Graph();

        assert.equal(graph.size(), 0);
    });

    it('has vertices', function() {
        var graph = new graphLib.Graph();
        assert.equal(graph.size(), 0);

        var a = {id:'a'}, b = {id:'b'};
        graph.addVertex(a);
        graph.addVertex(b);

        assert.equal(graph.size(), 2);
        assert.deepEqual(graph.vertices(), [a, b]);

    });

    it('manages incoming and outgoing vertices', function() {

        var a = {id:'a'}, b = {id:'b'}, c = {id:'c'};
        var graph = new graphLib.Graph();
        assert.equal(graph.size(), 0);

        graph.addVertex(a);        
        graph.addVertex(b); 
        graph.addVertex(c); 
        graph.addEdge(a,b);
        graph.addEdge(a,c);
        graph.addEdge(b,c);

        assert.equal(graph.size(), 3);
        assert.deepEqual(graph.outgoingVerticesOf(a), ['b', 'c']);
        assert.deepEqual(graph.outgoingVerticesOf(b), ['c']);
        assert.deepEqual(graph.outgoingVerticesOf(c), []);
        assert.deepEqual(graph.incomingVerticesOf(a), []);
        assert.deepEqual(graph.incomingVerticesOf(b), ['a']);
        assert.deepEqual(graph.incomingVerticesOf(c), ['a', 'b']);

        graph.removeVertex(b);

        assert.equal(graph.size(), 2);
        assert.deepEqual(graph.outgoingVerticesOf(a), ['c']);
        assert.deepEqual(graph.outgoingVerticesOf(b), []);
        assert.deepEqual(graph.outgoingVerticesOf(c), []);  
        assert.deepEqual(graph.incomingVerticesOf(a), []);
        assert.deepEqual(graph.incomingVerticesOf(b), []);
        assert.deepEqual(graph.incomingVerticesOf(c), ['a']);

        graph.removeVertex(a);

        assert.equal(graph.size(), 1);
        assert.deepEqual(graph.outgoingVerticesOf(a), []);
        assert.deepEqual(graph.outgoingVerticesOf(b), []);
        assert.deepEqual(graph.outgoingVerticesOf(c), []);
        assert.deepEqual(graph.incomingVerticesOf(a), []);
        assert.deepEqual(graph.incomingVerticesOf(b), []);
        assert.deepEqual(graph.incomingVerticesOf(c), []);

    });

    it('can replace vertices and maintain the edges', function() {

        var a = {id:'a'}, b = {id:'b'}, c = {id:'c'};
        var graph = new graphLib.Graph();

        graph.addVertex(a);        
        graph.addVertex(b); 
        graph.addVertex(c); 
        graph.addEdge(a,b);
        graph.addEdge(a,c);
        graph.addEdge(b,c);

        assert.deepEqual(graph.outgoingVerticesOf(a), ['b', 'c']);
        assert.deepEqual(graph.outgoingVerticesOf(b), ['c']);
        assert.deepEqual(graph.outgoingVerticesOf(c), []);
        assert.deepEqual(graph.incomingVerticesOf(a), []);
        assert.deepEqual(graph.incomingVerticesOf(b), ['a']);
        assert.deepEqual(graph.incomingVerticesOf(c), ['a', 'b']);

        var d = {id:'d'};
        graph.replaceVertex(b, d);

        assert.deepEqual(graph.outgoingVerticesOf(a), ['d', 'c']);
        assert.deepEqual(graph.outgoingVerticesOf(d), ['c']);
        assert.deepEqual(graph.outgoingVerticesOf(c), []);
        assert.deepEqual(graph.incomingVerticesOf(a), []);
        assert.deepEqual(graph.incomingVerticesOf(d), ['a']);
        assert.deepEqual(graph.incomingVerticesOf(c), ['a', 'd']);

    });

    it('can replace vertices with the same id', function() {

        var a = {id:'a'}, b1 = {id:'b'}, b2 = {id: 'b'}, c = {id:'c'};
        var graph = new graphLib.Graph();

        graph.addVertex(a);        
        graph.addVertex(b1); 
        graph.addVertex(c); 
        graph.addEdge(a,b1);
        graph.addEdge(a,c);
        graph.addEdge(b1,c);

        assert.deepEqual(graph.outgoingVerticesOf(a), ['b', 'c']);
        assert.deepEqual(graph.outgoingVerticesOf(b1), ['c']);
        assert.deepEqual(graph.outgoingVerticesOf(c), []);
        assert.deepEqual(graph.incomingVerticesOf(a), []);
        assert.deepEqual(graph.incomingVerticesOf(b1), ['a']);
        assert.deepEqual(graph.incomingVerticesOf(c), ['a', 'b']);

        graph.replaceVertex(b1, b2);

        assert.deepEqual(graph.outgoingVerticesOf(a), ['b', 'c']);
        assert.deepEqual(graph.outgoingVerticesOf(b2), ['c']);
        assert.deepEqual(graph.outgoingVerticesOf(c), []);
        assert.deepEqual(graph.incomingVerticesOf(a), []);
        assert.deepEqual(graph.incomingVerticesOf(b2), ['a']);
        assert.deepEqual(graph.incomingVerticesOf(c), ['a', 'b']);

    });

});

describe('GeometryGraph', function() {

    it('support a graph with one Point vertex', function() {
        var graph = new geometryGraph.Graph();
        var point = graph.createPointPrototype();

        assert.equal(graph.childrenOf(point).length, 0);        
    });

    it('can support a polyline with point children', function() {
        var graph = new geometryGraph.Graph();
        var polyline = graph.createPolylinePrototype();
        assert.equal(graph.childrenOf(polyline).length, 1); 

        graph.addPointToPolyline(polyline);
        assert.equal(graph.childrenOf(polyline).length, 2); 

    });

    it('can tell you the parents and children of a vertex', function() {

        var graph = new geometryGraph.Graph();
    });
})



