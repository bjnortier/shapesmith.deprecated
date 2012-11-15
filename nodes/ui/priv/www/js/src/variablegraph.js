define([
    'lib/parser',
    'src/graph',
    'src/geomnode',
    ], function(Parser, graphLib, geomNode) {

    var VariableGraph = function() {

        var graph = graph || new graphLib.Graph();
        var that = this;
        var parser = new Parser();

        var parseExpressionWithVariables = function(expression, variables) {
            try {
                return parser.parse(expression).evaluate(variables);
            } catch(e) {
                var re = new RegExp('undefined variable: (.*)+');
                var result = re.exec(e.message);
                if (result) {
                    throw new UnknownVariableError(result[1]);
                } else {
                    throw e;
                }
            }
        }

        this.evaluate = function(expression) {
            var variables = gatherVariables();  
            return parseExpressionWithVariables(expression, variables);
        }

        this.addVariable = function(name, expression) {
            var vertex = new geomNode.Variable({name: name, parameters: {expression: expression}});
            graph.addVertex(vertex);
            try {
                gatherVariables();
                return true;
            } catch (e) {
                return false;
            }
        }

        var gatherVariables = function() {
            var vars = {};
            var listener = function(vertex) {
                var variable = vertex.name;
                var expression = vertex.parameters.expression;
                var value = parseExpressionWithVariables(expression, vars);
                vars[variable] = value;
            }
            graph.leafFirstSearch(listener);
            return vars;
        }

    }

    function UnknownVariableError(variable) {
        Error.apply(this, arguments);
        this.message = variable;
    }

    UnknownVariableError.prototype = new Error();
    UnknownVariableError.prototype.constructor = UnknownVariableError;
    UnknownVariableError.prototype.name = 'UnknownVariableError';

    return {
        UnknownVariableError: UnknownVariableError,
        Graph: VariableGraph,
    }

});