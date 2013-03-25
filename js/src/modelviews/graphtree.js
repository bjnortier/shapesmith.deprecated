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
        'underscore',
        'src/geometrygraphsingleton',
        'src/modelviews/pointMV', 
        'src/modelviews/polylineMV',
        'src/modelviews/cubeMV',
        'src/modelviews/sphereMV',
        'src/modelviews/subtractMV',
    ], 
    function(
        _,
        geometryGraph,
        PointMV, 
        PolylineMV,
        CubeMV,
        SphereMV,
        SubtractMV) {

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
    var Node = function(vertex, model, domView, sceneView, children) {
        this.vertex = vertex;
        this.model = model;
        this.children = children || [];

        var that = this;
        this.children.forEach(function(child) {
            child.parent = this;            
        })

        this.dive = function() {
            this.domView.$el.find('> .children').show();
            this.children.forEach(function(child) {
                child.sceneView.show();
            })
            this.sceneView.hide();
        }

        this.ascend = function() {
            this.sceneView.show();
            this.domView.$el.find('.children').hide();
            var hideDescendants = function(node) {
                node.children.forEach(function(child) {
                    child.sceneView.hide();
                    child.domView.ascend();
                    hideDescendants(child);
                })
            }
            hideDescendants(this);
        }

        // Listen to dive/ascend events from the domview
        this.__defineSetter__('domView', function(domView) {
            if (this._domView) {
                this._domView.off('dive', this.dive, this);
                this._domView.off('ascend', this.ascend, this);
            }
            this._domView = domView;
            this._domView.on('dive', this.dive, this);
            this._domView.on('ascend', this.ascend, this);
        });

        this.__defineGetter__('domView', function(domView) {
            return this._domView;
        });

        this.domView = domView;
        this.sceneView = sceneView;
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
        this.domView.off('dive', this.dive, this);
        this.domView.off('ascend', this.ascend, this);
        this.model.destroy();
    }

    Node.prototype.getRoot = function() {
        if (this.parent) {
            return this.parent.getRoot();
        } else {
            return this;
        }
    }

    var addVertex = function(vertex) {
        if (shouldHaveTree(vertex)) {
            trees.push(createTree(vertex, $('#geometry')));
        } else if (vertex.implicit) {

            // Implicit vertices will be added as children to all 
            // parent nodes. This happens during editing when a new
            // point is added
            var parentVertices = geometryGraph.parentsOf(vertex);
            var parentNodes = [];
            parentVertices.forEach(function(parentVertex) {
                trees.forEach(function(tree) {
                    parentNodes = parentNodes.concat(tree.findVertex(parentVertex));
                })
            });
            parentNodes.forEach(function(parentNode) {
                var parentDomElement = parentNode.domView.$el.find('.children.' + parentNode.vertex.id);
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

        // If any of the children of the removed vertex should now be trees, 
        // create them
        var geomVerticesWithoutTrees = geometryGraph.filteredVertices(function(v) { 
            var isGeom = (v.category === 'geometry');
            var hasTree = _.find(trees, function(t) { return t.vertex.id === v.id});
            return isGeom && !hasTree;

        });
        geomVerticesWithoutTrees.forEach(function(v) {
            if (shouldHaveTree(v)) {
                trees.push(createTree(v, $('#geometry')));
            }
        });
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

            var newDOMView = newModel.addTreeView();
            newDOMView.$el.find('> .children.' + original.id).replaceWith(
                node.domView.$el.find('> .children,' + replacement.id));
            var sceneView = newModel.addSceneView();

            node.model.destroy();
            node.model = newModel;
            node.domView = newDOMView;
            node.sceneView = sceneView;

        });
    }

    // Doesn the vertex have a tree?
    var shouldHaveTree = function(vertex) {
        return !vertex.implicit && !geometryGraph.parentsOf(vertex).length;
    }

    var wrappers = {
        'point'     : PointMV,
        'polyline'  : PolylineMV,
        'cube'      : CubeMV,
        'sphere'    : SphereMV,
        'subtract'  : SubtractMV,
    }

    var createTree = function(vertex, domElement) {

        var modelCtor = vertex.editing ? 
            wrappers[vertex.type].EditingModel :
            wrappers[vertex.type].DisplayModel;
        var model = new modelCtor({
            vertex: vertex,
            appendDomElement: domElement,
        });

        var domView = model.addTreeView();
        var sceneView = model.addSceneView();
        // sceneView.hide();
        var childrenPlaceholder = domView.$el.find('> .children.' + vertex.id);
        return new Node(
            vertex, 
            model,
            domView,
            sceneView,
            geometryGraph.childrenOf(vertex).map(function(child) {

                // Look for existign trees that should become children, e.g.
                // when a boolean is created
                var foundTree = _.find(trees, function(t) {
                    return (t.vertex.id === child.id);
                });
                if (foundTree) {
                    trees.splice(trees.indexOf(foundTree), 1);
                    foundTree.domView.render();
                    childrenPlaceholder.append(foundTree.domView.$el);
                    foundTree.sceneView.hide();
                    return foundTree;
                } else {
                    var childTree = createTree(child, childrenPlaceholder);
                    childTree.sceneView.hide();
                    return childTree;
                }
            })
        )
    }

})