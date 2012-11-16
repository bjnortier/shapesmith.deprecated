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

        graph.addVariable('a', '1');
        assert.equal(graph.evaluate('a'), 1);

    });

    it('can evaluate a graph of variables', function() {

        graph.addVariable('a', '1');
        graph.addVariable('b', 'a + 1');
        graph.addVariable('c', 'b/4');

        assert.equal(graph.evaluate('a'), 1);
        assert.equal(graph.evaluate('b'), 2);
        assert.equal(graph.evaluate('c'), 0.5);

    });

    it('rejects invalid variables', function() {

        var added = graph.addVariable('a', '$^%&*');
        assert.isFalse(added);

    });

    it('returns the child variable');


});