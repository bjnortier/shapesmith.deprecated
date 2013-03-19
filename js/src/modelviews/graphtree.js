// The graph tree is a tree-like interface to a graph. Trees are much
// easier to display and interact with than graphs, since they are simple
// collapsable hierarchies. 
//
// Representing a graph (a DAG to be exact) using trees leads to redundancy -
// a vertex in the graph can be represented by more than one node in the tree.
// 
// The graph tree keeps track, in a non-durable way, of which nodes have been 
// drilled down to.
//
// Rules:
//   1. Each vertex in the graph that has no parents and is not implicit 
//      will have a tree.
//   2. Vertices in the graph that are shared between more than one resulting
//      tree will be duplicated.

define([
        'src/geometrygraphsingleton',
        'src/modelviews/pointMV', 
        'src/modelviews/polylineMV',
    ], 
    function(
        geometryGraph,
        PointMV, 
        PolylineMV) {

    geometryGraph.on('vertexAdded', function(vertex) {
        if (vertex.category === 'geometry') {
            addVertex(vertex);
        }
    });

    geometryGraph.on('vertexRemoved', function(vertex) {
        if (vertex.category === 'geometry') {
            removeVertex(vertex);
        }
    });

    geometryGraph.on('vertexReplaced', function(original, replacement) {
        if (replacement.category === 'geometry') {
            replaceVertex(original, replacement);
        }
    }); 

    var trees = [];

    // A tree node
    var Node = function(vertex, model, children) {
        this.vertex = vertex;
        this.model = model;
        this.children = children || [];
    }

    Node.prototype.findVertex = function(vertex) {
        var found = [];
        if (vertex.id === this.vertex.id) {
            found.push(this);
        }
        this.children.forEach(function(child) {
            found = found.concat(child.findVertex(vertex));
        })
        return found;
    }

    Node.prototype.remove = function() {
        this.children.forEach(function(child) {
            child.remove();
        })
        this.model.destroy();
    }

    var addVertex = function(vertex) {
        if (hasTree(vertex)) {
            trees.push(createTree(vertex, $('#geometry')));
        } else if (vertex.implicit) {
            // Implicit vertices will be added as children to all 
            // parent nodes
            var parentVertices = geometryGraph.parentsOf(vertex);
            var parentNodes = [];
            parentVertices.forEach(function(parentVertex) {
                trees.forEach(function(tree) {
                    parentNodes = parentNodes.concat(tree.findVertex(parentVertex));
                })
            });

            parentNodes.forEach(function(parentNode) {
                var parentDomElement = parentNode.model.domView.$el.find('.children');
                parentNode.children.push(createTree(vertex, parentDomElement));
            })
        }
    }

    var removeVertex = function(vertex) {
        trees = trees.reduce(function(acc, root) {
            root.findVertex(vertex).forEach(function(node) {
                node.remove();
            })
            if (root.vertex.id === vertex.id) {
                return acc;
            } else {
                return acc.concat(root);
            }
        }, []);
    }

    // Find the nodes in the tree representing the vertex and replace
    // with the new model
    var replaceVertex = function(original, replacement) {
        var nodes = [];
        trees.forEach(function(node) {
            nodes = nodes.concat(node.findVertex(original));
        })
        nodes.forEach(function(node) {
            var modelCtor = replacement.editing ? 
                wrappers[replacement.type].EditingModel :
                wrappers[replacement.type].DisplayModel;
            var replaceDomElement = node.model.domView.$el;

            var newModel = new modelCtor({
                original: original, 
                vertex: replacement,
                replaceDomElement: replaceDomElement,
            });

            node.model.destroy();
            node.model = newModel;

        });
    }

    // Doesn the vertex have a tree?
    var hasTree = function(vertex) {
        return !vertex.implicit && !geometryGraph.parentsOf(vertex).length;
    }

    var wrappers = {
        'point'     : PointMV,
        'polyline'  : PolylineMV,
    }

    var createTree = function(vertex, domElement) {

        var modelCtor = vertex.editing ? 
            wrappers[vertex.type].EditingModel :
            wrappers[vertex.type].DisplayModel;
        var model = new modelCtor({
            vertex: vertex,
            appendDomElement: domElement,
        });

        return new Node(
            vertex, 
            model,
            geometryGraph.childrenOf(vertex).map(function(child) {
                var parentDomElement = model.domView.$el.find('.children');
                return createTree(child, parentDomElement);
            })
        )
    }

})