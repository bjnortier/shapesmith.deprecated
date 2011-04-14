
function error_response(responseText) {
    var error = JSON.parse(responseText);
    $('tr.field').removeClass('validation-error');
    if (error.validation) {
	for (var i in error.validation) {
	    $('#' + i).parents('tr.field').addClass('validation-error');
	}
    }
    command_stack.inProgressFailure(error);
}


function update_geom_command(fromNode, toNode) {
    
    var chainedPutFn = function(fromChain, toChain) {
        // TODO: Replace with array copy
        fromChain = fromChain.map(function(x) { return x });
        toChain = toChain.map(function(x) { return x });
        var nextTo = toChain.splice(0,1)[0];
        var nextFrom = fromChain.splice(0,1)[0];
        if (nextTo) {
            $.ajax({
                type: 'PUT',
                url: nextTo.path,
                contentType: 'application/json',
                data: nextTo.toShallowJson(),
                success: function(nodeData) {
                    if (nextTo.editing) {
                        nextTo.editing = false;
                    }
                    for (var i in nextTo.transforms) {
                        if (nextTo.transforms[i].editing) {
                            nextTo.transforms[i].editing = false;
                        }
                    }
                    if (toChain.length > 0) {
                        chainedPutFn(fromChain, toChain);
                    } else {
                        // No more -> update the root node
                        $.ajax({
                            type: 'GET',
                            url: '/mesh/' + idForGeomPath(nextFrom.path),
                            success: function(mesh) {
                                nextTo.mesh = mesh;
                                selectionManager.deselectAll();
                                geom_doc.replace(nextFrom, nextTo);
				command_stack.inProgressSuccess();
                            },
			    error: function(jqXHR, textStatus, errorThrown) {
				error_response(jqXHR.responseText);
			    }

                        });
                    }
                },
		error: function(jqXHR, textStatus, errorThrown) {
		    error_response(jqXHR.responseText);
                }
            });
        }
    }

    var ancestors = geom_doc.ancestors(toNode);
    var ancestorCopies = ancestors.map(function(ancestor) {
        return ancestor.editableCopy();
    });

    var toChain = [toNode].concat(ancestors);
    var fromChain = [fromNode].concat(ancestorCopies);

    var doFn = function() {
        chainedPutFn(fromChain, toChain);
    };
    var undoFn = function() {
        chainedPutFn(toChain, fromChain);
    };
    var redoFn = function() {
        chainedPutFn(fromChain, toChain);
    }

    return new Command(doFn, undoFn, redoFn);
}


function create_geom_command(prototype, geometry) {
    
    var id;
    var geomNode;
    
    var doFn = function() {
        $.ajax({
            type: 'POST',
            url: '/geom/',
            contentType: 'application/json',
            data: JSON.stringify(geometry),
	    dataType: 'json',
            success: function(nodeData){
                var path = nodeData.path;
                id = idForGeomPath(nodeData.path);
                $.ajax({
                    type: 'GET',
                    url: '/mesh/' + id,
		    dataType: 'json',
                    success: function(mesh) {
                        geomNode = new GeomNode({
                            type : geometry.type,
                            path : path,
                            parameters : geometry.parameters,
                            mesh : mesh})
                        selectionManager.deselectAll();
                        geom_doc.replace(prototype, geomNode);
                        command_stack.inProgressSuccess();
                    },
                    error: function(jqXHR, textStatus, errorThrown) {
			error_response(jqXHR.responseText);
                    }
                });
            },
            error: function(jqXHR, textStatus, errorThrown) {
                error_response(jqXHR.responseText);
            }
        });
    };
    var undoFn = function() {
        geom_doc.remove(geomNode);
	command_stack.inProgressSuccess();
    }
    var redoFn = function() {
        geom_doc.add(geomNode);
	command_stack.inProgressSuccess();
    }

    return new Command(doFn, undoFn, redoFn);
}


function boolean(type) {
    if ((type == 'union') || (type == 'intersect')) {
        if (selectionManager.size() <= 1)  {
            alert("must have > 2 object selected!");
            return;
        }
    } else if (type =='subtract') {
        if (selectionManager.size() != 2)  {
            alert("must have 2 object selected!");
            return;
        }
    }

    var id;
    var boolNode;
    var childNodes;

    var doFn = function() {
        var selected = selectionManager.selected();
        var geometry = {type: type,
                        children: selected
                       };
        
        $.ajax({
            type: "POST",
            url: "/geom/",
            contentType: "application/json",
            data: JSON.stringify(geometry),
            success: function(nodeData) {
                var path = nodeData.path;
                id = id = idForGeomPath(nodeData.path);
                $.ajax({
                    type: "GET",
                    url: '/mesh/' + id,
                    success: function(mesh) {
                        childNodes = selected.map(function(x) {
                            var node = geom_doc.findByPath(x);
                            geom_doc.remove(node);
                            return node;
                        });
                        geometry["path"] = path;
                        boolNode = new GeomNode(geometry, childNodes);
                        boolNode.mesh = mesh;
                        selectionManager.deselectAll();
                        geom_doc.add(boolNode);
			command_stack.inProgressSuccess();
                    },
		    error: function(jqXHR, textStatus, errorThrown) {
			error_response(jqXHR.responseText);
		    }
                });
            },
            error: function(jqXHR, textStatus, errorThrown) {
                error_response(jqXHR.responseText);
            }
        })};

    var undoFn = function() {
        geom_doc.remove(boolNode);
        childNodes.reverse().map(function(x) {
            geom_doc.add(x);
        });
	command_stack.inProgressSuccess();
    }

    var redoFn = function() {
        childNodes.map(function(x) {
            geom_doc.remove(x);
        });
        geom_doc.add(boolNode);
	command_stack.inProgressSuccess();
    }

    var cmd = new Command(doFn, undoFn, redoFn);
    command_stack.execute(cmd);
}


function save() {
    var docId = $.getQueryParam("docid");
    var rootPaths = geom_doc.rootNodes.filter(function(x) {
        return !x.editing;
    }).map(function(x) {
        return x.path;
    });
    console.log(rootPaths);
    $.ajax({
        type: 'PUT',
        url: '/doc/' + docId,
        contentType: 'application/json',
        data: JSON.stringify(rootPaths),
        success: function() {
            console.log('saved');
	    renderSuccessMessage('Saved');
        },
        error: function(jqXHR, textStatus, errorThrown) {
            error_response(jqXHR.responseText);
        }
    });
}

function load(docId) {
    $.ajax({
        type: 'GET',
        url: '/doc/' + docId,
        dataType: 'json',
        success: function(geomPaths) {
            geomPaths.map(function(path) {
                console.log("loading " + path);
                $.ajax({
                    type: 'GET',
                    url: path + '?recursive=true',
                    dataType: 'json',
                    success: function(geomJson) {
                        var newNode = GeomNode.fromDeepJson(geomJson);
                        $.ajax({
                            type: 'GET',
                            url: '/mesh/' + idForGeomPath(path),
                            success: function(mesh) {
                                newNode.mesh = mesh;
                                geom_doc.add(newNode);
                            },
			    error: function(jqXHR, textStatus, errorThrown) {
				error_response(jqXHR.responseText);
			    }
                        });

                    },
		    error: function(jqXHR, textStatus, errorThrown) {
			error_response(jqXHR.responseText);
		    }
                });
            });
        },
        error: function(jqXHR, textStatus, errorThrown) {
            error_response(jqXHR.responseText);
        }
    });

}
