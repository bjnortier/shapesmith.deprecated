var chai = require('chai'),
    assert = chai.assert,
    requirejs = require('requirejs');

chai.Assertion.includeStack = true;

requirejs.config({
    baseUrl: '..',
    nodeRequire: require,
});

var geomNode = requirejs('src/geomnode');
var geometryGraph = requirejs('src/geometrygraph');

describe('GeometryGraph', function() {

    beforeEach(function() {
        geomNode.resetIDCounters();
    });

    // ---------- Cases ----------

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

    it('can be serialized', function() {
        var a = new geomNode.Point();
        var b = new geomNode.Point();
        var c = new geomNode.Polyline();
        var graph = new geometryGraph.Graph();

        graph.add(c);
        graph.addChildTo(c,a);
        graph.addChildTo(c,b);

        a.sha = 'aa';
        b.sha = 'bb';
        c.sha = 'cc';

        assert.deepEqual(graph.serialize(), {
            edges: {
                'cc': ['aa', 'bb'],
                'bb': [],
                'aa': [],
            }
        });
    });

    it('can add and update variables', function() {

        var graph = new geometryGraph.Graph();
        var a = graph.addVariable('a', '1');
        assert.equal(a.id, 'a');    

        assert.isUndefined(graph.updateVariable('a', 'b', ''));
        assert.isObject(graph.updateVariable('a', 'b', '2'));
        assert.isObject(graph.updateVariable('a', 'a', '2'));
    })

})
