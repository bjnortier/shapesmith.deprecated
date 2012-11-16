var chai = require('chai'),
    assert = chai.assert,
    requirejs = require('requirejs');

chai.Assertion.includeStack = true;

requirejs.config({
    baseUrl: '..',
    nodeRequire: require,
});

var graphLib = requirejs('src/graph');

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

        assert.deepEqual(graph.vertexById('a'), a);

        assert.throws(function() {
            graph.addVertex(a);
        }, Error);

    });

    it('can check for name uniqueness', function() {
        var graph = new graphLib.Graph();
        var a = {id:'a', name: 'aa'}, b = {id:'b', name: 'bb'};
        graph.addVertex(a);
        graph.addVertex(b);

        assert.isTrue(graph.nameIsTaken('aa'))
        assert.isFalse(graph.nameIsTaken('cc'))

    });

    it('can fetch by name for name', function() {
        var graph = new graphLib.Graph();
        var a = {id:'a', name: 'aa'}, b = {id:'b', name: 'bb'};
        graph.addVertex(a);
        graph.addVertex(b);

        assert.deepEqual(graph.vertexByName('aa'), a);
        assert.deepEqual(graph.vertexByName('bb'), b);
        assert.isUndefined(graph.vertexByName('e'))

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

    it('can add and remove edges', function() {

        var a = {id:'a'}, b1 = {id:'b1'}, b2 = {id: 'b2'};
        var graph = new graphLib.Graph();

        graph.addVertex(a);        
        graph.addVertex(b1); 
        graph.addVertex(b2); 
        graph.addEdge(a,b1);
        graph.addEdge(a,b2);

        assert.deepEqual(graph.outgoingVerticesOf(a), ['b1', 'b2']);
        assert.deepEqual(graph.outgoingVerticesOf(b1), []);
        assert.deepEqual(graph.outgoingVerticesOf(b2), []);

        graph.removeEdge(a, b1);

        assert.deepEqual(graph.outgoingVerticesOf(a), ['b2']);
        assert.deepEqual(graph.outgoingVerticesOf(b1), []);
        assert.deepEqual(graph.outgoingVerticesOf(b2), []);
        assert.deepEqual(graph.incomingVerticesOf(b1), []);
        assert.deepEqual(graph.incomingVerticesOf(b2), ['a']);

        assert.throws(function() {
            graph.removeEdge(a, b1);
        }, Error);

    });

    it('can do a leaf-first search of all vertices', function() {

        var a = {id:'a'}, b1 = {id:'b1'}, b2 = {id: 'b2'}, c = {id:'c'}, d = {id:'d'};
        var graph = new graphLib.Graph();

        graph.addVertex(a);        
        graph.addVertex(b1); 
        graph.addVertex(b2); 
        graph.addEdge(a,b1);
        graph.addEdge(a,b2)
        graph.addVertex(c);
        graph.addVertex(d);
        graph.addEdge(c,d);

        var sequence = [];
        var listener = function(vertex) {
            sequence.push(vertex.id);
        }

        graph.leafFirstSearch(listener);

        assert.deepEqual(sequence, ['b1','b2','a','d','c']);
    });

});