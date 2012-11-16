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

var variableGraph = requirejs('src/variablegraph');

describe('Expressions', function() {

    var graph;

    beforeEach(function() {
        graph = new variableGraph.Graph();
    });

    // ---------- Cases ----------

    it('can evaluate expressions without variables', function() {

        assert.equal(graph.evaluate(55), 55);
        assert.equal(graph.evaluate('1 + 2'), 3);
        assert.equal(graph.evaluate('1/2'), 0.5);
        assert.equal(graph.evaluate('PI').toFixed(5), '3.14159');

        assert.throws(function() {
            graph.evaluate('$%^&*(');
        }, variableGraph.ParseError);

        assert.throws(function() {
            graph.evaluate('');
        }, variableGraph.ParseError);


    });

    it('throws an error if a undefined variable is used', function() {

        try {
            graph.evaluate('x');
            assert.fail();
        } catch (e) {
            assert.isTrue(e instanceof variableGraph.UnknownVariableError);
            assert.equal(e.message, 'x');
        }
    });

    it('can evaluate a defined variable', function() {

        graph.add(variableGraph.createVertex('a', '1'));
        assert.equal(graph.evaluate('a'), 1);

    });

    it('can evaluate a graph of variables', function() {

        graph.add(variableGraph.createVertex('a', '1'));
        graph.add(variableGraph.createVertex('b', 'a + 1'));
        graph.add(variableGraph.createVertex('c', 'b/4'));

        assert.equal(graph.evaluate('a'), 1);
        assert.equal(graph.evaluate('b'), 2);
        assert.equal(graph.evaluate('c'), 0.5);

    });

    // it('can update a variable with a new name', function() {
    //     var vertex = graph.add(variableGraph.createVertex('a', '1'));

    //     var newVertex = graph.updateVariable('a', 'b', '2');
    //     assert.isObject(newVertex);

    // });

    // it('can update a variable with the same name', function() {
    //     var vertex = graph.addVariable('a', '1');

    //     var newVertex = graph.updateVariable('a', 'a', '2');
    //     assert.isObject(newVertex);
    // });

    // it('rejects invalid updates', function() {

    //     var vertex = graph.addVariable('a', '1');

    //     assert.throws(function() {
    //         graph.updateVariable('x', 'b', '1');
    //     }, variableGraph.UnknownVariableError);

    //     assert.isUndefined(graph.updateVariable('a', 'b', ''));
    //     assert.isUndefined(graph.updateVariable('a', 'a', ''));

    // });

    it('can determine if a potential new variable is valid', function() {

        var a = variableGraph.createVertex('a', '1');
        var b = variableGraph.createVertex('b', '1*5');
        
        assert.isTrue(graph.canAdd(a));
        assert.isTrue(graph.canAdd(b));

        graph.add(a);
        var a2 = variableGraph.createVertex('a', '15*5')
        assert.isTrue(graph.canAdd(a2)); // Replace editing with non-editing

        assert.isFalse(graph.canAdd(variableGraph.createVertex('c', 'r*5')));
        assert.isFalse(graph.canAdd(variableGraph.createVertex('a', '')));

        var e = variableGraph.createVertex('e', '24332');
        e.name = '';
        assert.isFalse(graph.canAdd(e));

    });

    it('rejects variable name change when it has parents in graph');

    it('returns the child variable');


});