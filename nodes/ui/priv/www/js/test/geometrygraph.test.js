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

    it('has SHA values for each vertex', function() {
        var a = new geomNode.Point();
        var b = new geomNode.Point();
        var graph = new geometryGraph.Graph();

        graph.add(a);
        graph.add(b);

        assert.equal(graph.shaOfVertexWithId(a.id), 'f02a760e9d32b4de98da156194d26184c8ed823c');
        assert.equal(graph.shaOfVertexWithId(b.id), '84a560183a67d3615ebab972847e067d3f39c681');

        // Vertices with the same name should generate the same hash
        // (vertices with duplicate names are not allowed in th graph)
        a.name = 'point0';
        b.name = 'point0';

        assert.equal(graph.shaOfVertexWithId(a.id), 'f02a760e9d32b4de98da156194d26184c8ed823c');
        assert.equal(graph.shaOfVertexWithId(b.id), 'f02a760e9d32b4de98da156194d26184c8ed823c');
    });

    it('has SHA values for vertices with children', function() {
        var a = new geomNode.Point();
        var b = new geomNode.Point();
        var c = new geomNode.Polyline();
        var d = new geomNode.Polyline();
        var graph = new geometryGraph.Graph();


        graph.add(a);
        graph.add(b);
        graph.add(c);
        graph.add(d);

        graph.addPointToPolyline(c, a);
        graph.addPointToPolyline(c, b);

        graph.addPointToPolyline(d, a);
        graph.addPointToPolyline(d, b);        

        assert.equal(graph.shaOfVertexWithId(c.id), '4a8b1c910faa03578e58975824ee38fddfddab1a');
        assert.equal(graph.shaOfVertexWithId(d.id), 'cb5f774294c93784843288a8111ced82c1089b44');

        d.id = c.id;
        assert.equal(graph.shaOfVertexWithId(d.id), '4a8b1c910faa03578e58975824ee38fddfddab1a');

    });
})
