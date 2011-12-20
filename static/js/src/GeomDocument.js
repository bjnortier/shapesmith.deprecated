
function GeomDocument() {

    this.rootNodes = [];

    this.isRoot = function(node) {
        return this.rootNodes.indexOf(node) > -1;
    }
    
    this.add = function(node) {
        this.rootNodes = [node].concat(this.rootNodes);
        this.notify({add: node});
    }

    this.removeAll = function() {
	for (index in this.rootNodes) {
	    this.notify({remove: this.rootNodes[index]});
	}
	this.rootNodes = [];
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

    this.removeById = function(id) {
        var toRemove = [];
        for (var i in this.rootNodes) {
            if (this.rootNodes[i].id == id) {
                toRemove.push(this.rootNodes[i]);
            }
        }
        for (var i in toRemove) {
            this.remove(toRemove[i]);
        }
    }

    this.findById = function(id) {
        var recurFn = function(geomNode) {
            if (geomNode.id == id) {
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

    this.getPreviewNode = function() {
	for(i in this.rootNodes) {
	    if (this.rootNodes[i].editing) {
		return this.rootNodes[i];
	    }
	}
	return null;
    }

    Observable.makeObservable(this);
}