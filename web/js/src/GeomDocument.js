
function GeomDocument() {

    this.rootNodes = [];

    this.isRoot = function(node) {
        return this.rootNodes.indexOf(node) > -1;
    }
    
    this.add = function(node) {
        this.rootNodes = [node].concat(this.rootNodes);
        this.notify({add: node});
    }

    this.remove = function(node) {
        this.rootNodes.splice(this.rootNodes.indexOf(node),1);
        this.notify({remove: node});
    }

    this.replace = function(original, replacement) {
        var recurFn = function(children) {
            var index = children.indexOf(original);
            if (index > -1 ) {
                children.splice(index,1,replacement);
            } else {
                for (i in children) {
                    recurFn(children[i].children);
                }
            }
        }
        recurFn(this.rootNodes);
        this.notify({replace: {original : original,
                               replacement : replacement}});
    }

    this.removeByPath = function(path) {
        var toRemove = [];
        for (var i in this.rootNodes) {
            if (this.rootNodes[i].path == path) {
                toRemove.push(this.rootNodes[i]);
            }
        }
        for (var i in toRemove) {
            this.remove(toRemove[i]);
        }
    }

    this.findByPath = function(path) {
        var recurFn = function(geomNode) {
            if (geomNode.path == path) {
                return geomNode;
            } else {
                for (var i in geomNode.children) {
                    var foundChild = recurFn(geomNode.children[i]);
                    if (foundChild) {
                        return foundChild;
                    } 
                }
                return null;
            }
        }
        for (var i in this.rootNodes) {
            var found = recurFn(this.rootNodes[i]);
            if (found) {
                return found;
            }
        }
        return null;
    }

    this.ancestors = function(nodeToFind) {
        var recurFn = function(recurNode, ancestors) {
            if (recurNode == nodeToFind) {
                return ancestors;
            } else {
                var newAncestors = ancestors.slice();
                newAncestors.push(recurNode);

                for (i in recurNode.children) {
                    var child = recurNode.children[i];
                    var childResult = recurFn(child, newAncestors);
                    if (childResult) {
                        return childResult;
                    }
                }
            }
            return null;
        }
        for (var i in this.rootNodes) {
            var result = recurFn(this.rootNodes[i], []);
            if (result) {
                return result.reverse();
            }
        }
        throw Error("node not found");
    }

    this.toJson = function() {

        return this.rootNodes.map(function(x) {
            return JSON.parse(x.toDeepJson());
        });
    }

    Observable.makeObservable(this);
}