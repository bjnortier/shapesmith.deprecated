define([
        'src/geomnode',
        'src/geometrygraphsingleton',
        'src/command',
        'src/commandstack',
    ], function(geomNode, geometryGraph, Command, commandStack) {

    var post = function(url, data, successFn, errorFn) {
        $.ajax({
            type: 'POST',
            url: url,
            contentType: 'application/json',
            data: data,
            dataType: 'json',
            success: successFn,
            error: errorFn,
        });
    }

    var get = function(url, successFn, errorFn) {
        $.ajax({
            type: 'GET',
            url: url,
            dataType: 'json',
            success: successFn,
            error: errorFn,
        });
    }

    var captureVertices = function(vertices, callback) {
        var url = '/' + SS.session.username + '/' + SS.session.design + '/vertex/';
        var shas = vertices.map(function() {
            return undefined;
        })
        vertices.forEach(function(v, i) {
            post(url, v.toJSON(), function(sha) {
                v.sha = sha;
                shas[i] = sha;
                var someRemaining = _.any(shas, function(sha) {
                    return sha === undefined;
                });
                if (!someRemaining) {
                    callback(shas);
                }
            });
        });
    }

    var captureGraph = function(callback) {
        var url = '/' + SS.session.username + '/' + SS.session.design + '/graph/';
        post(url, JSON.stringify(geometryGraph.serialize()), callback);
    }
    
    var tryCommitCreate = function(editingVertices, callback) {
        var allOk = true;
        editingVertices.forEach(function(v) {
            if (!geometryGraph.validate(v)) {
                allOk = false;
            }
        });
        if (!allOk) {
            callback({'error' : 'validation'});
            return;
        }

        var nonEditingVertices = editingVertices.map(function(v) {
            return v.cloneNonEditing();
        });

        var doFn = function(commandSuccessFn, commandErrorFn) {
            captureVertices(nonEditingVertices, function() {
                for (var i = 0; i < editingVertices.length; ++i) {
                    geometryGraph.replace(editingVertices[i], nonEditingVertices[i]);
                }
                captureGraph(commandSuccessFn);
            });
        }

        var undoFn = function() {
            nonEditingVertices.forEach(function(v) {
                geometryGraph.remove(v);
            });
        }

        var redoFn = function() {
            nonEditingVertices.forEach(function(v) {
                geometryGraph.add(v);
            })
        }

        var command = new Command(doFn, undoFn, redoFn);
        commandStack.do(command, callback, nonEditingVertices);
    }

    var tryCommitEdit = function(originalVertices, editingVertices, callback) {
        var allOk = true;
        editingVertices.forEach(function(v) {
            if (!geometryGraph.validate(v)) {
                allOk = false;
            }
        });
        if (!allOk) {
            callback({'error' : 'validation'});
            return;
        }

        var allAreSame = true;
        for (var i = 0; i < editingVertices.length; ++i) {
            if (!originalVertices[i].hasSameJSON(editingVertices[i])) {
                allAreSame = false;
                break;
            }
        }
        if (allAreSame) {
            editingVertices.map(function(editingVertex, i) {
                geometryGraph.replace(editingVertex, originalVertices[i]);
            });
            callback({newVertices: originalVertices});
            return;
        }

        var nonEditingVertices = editingVertices.map(function(v) {
            return v.cloneNonEditing();
        });

        var doFn = function(commandSuccessFn, commandErrorFn) {

            captureVertices(nonEditingVertices, function(shas) {
                for (var i = 0; i < editingVertices.length; ++i) {
                    geometryGraph.replace(editingVertices[i], nonEditingVertices[i]);
                }
                captureGraph(commandSuccessFn);
            });
        }

        var undoFn = function() {
            for (var i = 0; i < nonEditingVertices.length; ++i) {
                geometryGraph.replace(nonEditingVertices[i], originalVertices[i]);
            }
        }

        var redoFn = function() {
            for (var i = 0; i < originalVertices.length; ++i) {
                geometryGraph.replace(originalVertices[i], nonEditingVertices[i]);
            }
        }

        var command = new Command(doFn, undoFn, redoFn);
        commandStack.do(command, callback, nonEditingVertices);

    }

    var tryCommitDelete = function(vertex, callback) {

        var children = _.uniq(geometryGraph.childrenOf(vertex));
        var parents =  geometryGraph.parentsOf(vertex);

        if (parents.length > 0) {
            callback &&     callback({error: 'cannot delete vertex with parents'});
            return;
        }

        var doFn = function(commandSuccessFn, commandErrorFn) {
            geometryGraph.remove(vertex);

            var removedChildren = [];
            children.forEach(function(child) {
                // Avoid removing shared children more than once
                if (child.implicit && (removedChildren.indexOf(child) === -1)) {
                    geometryGraph.remove(child);
                    removedChildren.push(child);
                } 
            })
            captureGraph(commandSuccessFn);
        }

        var undoFn = function() {
            // Editing is set to false for both the vertex
            // and children for deleting whilst editing

            var addedChildren = [];
            children.forEach(function(child) {
                if (child.implicit && (addedChildren.indexOf(child) === -1)) {
                    child.editing = false;
                    geometryGraph.add(child);
                    addedChildren.push(child);
                }
            })
            
            vertex.editing = false;
            geometryGraph.add(vertex, function() {
                children.forEach(function(child) {
                    geometryGraph.addEdge(vertex, child);
                });
            });
        }

        var redoFn = function() {
            geometryGraph.remove(vertex);

            var removedChildren = [];
            children.forEach(function(child) {
                // Avoid removing shared children more than once
                if (child.implicit && (removedChildren.indexOf(child) === -1)) {
                    geometryGraph.remove(child);
                    removedChildren.push(child);
                } 
            })
        }

        var command = new Command(doFn, undoFn, redoFn);
        commandStack.do(command, callback, []);

    }

    var cancelCreate = function(vertex) {
        geometryGraph.remove(vertex);
    }

    var cancelEdit = function(editingVertices, originalVertices) {
        editingVertices.forEach(function(editingVertex, i) {
            geometryGraph.replace(editingVertex, originalVertices[i]);
        });
    }

    var edit = function(vertex) {
        var editingReplacement = vertex.cloneEditing();
        geometryGraph.replace(vertex, editingReplacement);
        return editingReplacement;
    }

    var loadFromCommit = function(commit) {
        var url = '/' + SS.session.username + '/' + SS.session.design + '/graph/' + commit;
        geometryGraph.removeAll();
        get(url, function(graph) {
            var vertexSHAsToLoad = _.keys(graph.edges);
            var remaining = vertexSHAsToLoad.length;
            shasToVertices = {};

            var vertexCreated = function(sha, vertex) {
                vertex.sha = sha;
                shasToVertices[sha] = vertex;
                --remaining;
                if (remaining == 0) {
                    geometryGraph.createGraph(graph, shasToVertices);
                    loadFinished();
                }
            }

            if (vertexSHAsToLoad.length > 0) {
                vertexSHAsToLoad.forEach(function(sha) {
                    get('/' + SS.session.username + '/' + SS.session.design + '/vertex/' + sha,
                        function(data) {
                            var vertex = new geomNode.constructors[data.type](data);
                            vertexCreated(sha, vertex);
                        },
                        function(msg) {
                            console.log('error fetching vertex', sha);
                            --remaining;
                        });
                });
            } else {
                loadFinished();
            }

        });
    }

    // This is for webdriver to determine when things have loaded
    var loadFinished = function() {
        // Add a workplane if there is none
        var workplaneVertices = geometryGraph.filteredVertices(function(v) {
            return v.type === 'workplane';
        });
        if (workplaneVertices.length === 0) {
            geometryGraph.add(new geomNode.Workplane());
        }

        if (!SS.loadDone) {
            SS.loadDone = true;
        }
    }

    return {
        edit            : edit,
        tryCommitEdit   : tryCommitEdit,
        tryCommitCreate : tryCommitCreate,
        tryCommitDelete : tryCommitDelete,
        cancelEdit      : cancelEdit,
        cancelCreate    : cancelCreate,
        loadFromCommit  : loadFromCommit,
    }

});