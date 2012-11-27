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
        // if (!this.validate(editingVertex)) {
        //     return;
        // }

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

    var cancelCreate = function(vertex) {
        geometryGraph.remove(vertex);
    }

    var cancelEdit = function(original, vertex) {
        geometryGraph.replace(vertex, original);
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
        cancelEdit      : cancelEdit,
        cancelCreate    : cancelCreate,
        loadFromCommit  : loadFromCommit,
    }

});