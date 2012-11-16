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
                if (typeof(expression) === 'number') {
                    return expression;
                } else {
                    return parser.parse(expression).evaluate(variables);
                }
            } catch(e) {
                var re = new RegExp('undefined variable: (.*)+');
                var result = re.exec(e.message);
                if (result) {
                    throw new UnknownVariableError(result[1]);
                } else {
                    throw new ParseError();
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
                return vertex;
            } catch (e) {
                graph.removeVertex(vertex);
                return undefined;
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

    function ParseError() {
        Error.apply(this, arguments);
    }

    ParseError.prototype = new Error();
    ParseError.prototype.constructor = ParseError;
    ParseError.prototype.name = 'ParseError';

    return {
        UnknownVariableError: UnknownVariableError,
        ParseError: ParseError,
        Graph: VariableGraph,
    }

});