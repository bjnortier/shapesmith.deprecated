var SS = SS || {};

function error_response(responseText) {
    var error;
    try {
	var error = JSON.parse(responseText);
	$('tr.field').removeClass('validation-error');
	if (error.validation) {
	    for (var i in error.validation) {
		$('#' + i).parents('tr.field').addClass('validation-error');
	    }
	}
    } catch (e) {
	error = {exception: e};
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
	selectionManager.deselectAll();
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
    var geomNode;
    var doFn = function() {
        $.ajax({
            type: 'POST',
            url: '/' + SS.session.username + '/' + SS.session.design + '/geom/',
            contentType: 'application/json',
            data: JSON.stringify(geometry),
	    dataType: 'json',
            success: function(result) {
		var sha = result.SHA;
		var path = result.path;
                $.ajax({
                    type: 'GET',
                    url: '/' + SS.session.username + '/' + SS.session.design + '/mesh/' + sha,
		    dataType: 'json',
                    success: function(mesh) {
                        selectionManager.deselectAll();

			geometry.sha = sha;
                        geomNode = new GeomNode(geometry);
                        geomNode.mesh = mesh;
			geomNode.path = path;

                        geom_doc.replace(prototype, geomNode);
                        command_stack.commit();
                    },
                    error: function(jqXHR, textStatus, errorThrown) {
			command_stack.error(jqXHR.responseText);
                    }
                });
            },
            error: function(jqXHR, textStatus, errorThrown) {
                command_stack.error(jqXHR.responseText);
            }
        });
    };
    var undoFn = function() {
        geom_doc.remove(geomNode);
	command_stack.success();
    }
    var redoFn = function() {
        geom_doc.add(geomNode);
	command_stack.success();
    }

    return new Command(doFn, undoFn, redoFn);
}


function boolean(selected, type) {
    if ((type == 'union') || (type == 'intersect')) {
        if (selected.length <= 1)  {
            alert("must have > 2 object selected!");
            return;
        }
    } else if (type =='subtract') {
        if (selected.length != 2)  {
            alert("must have 2 object selected!");
            return;
        }
    }

    var sha;
    var boolNode;
    var childNodes;

    var doFn = function() {
        var geometry = {type: type,
                        children: selected.map(function(id) {
			    var pattern = /^([0-9]+)_(.*)$/;
			    var match = id.match(pattern);
			    return match[2];
			})};
        $.ajax({
            type: "POST",
	    url: '/' + SS.session.username + '/' + SS.session.design + '/geom/',
            contentType: "application/json",
            data: JSON.stringify(geometry),
            success: function(result) {
		var sha = result.SHA;
		geometry.sha = sha;
                $.ajax({
                    type: "GET",
                    url: '/' + SS.session.username + '/' + SS.session.design + '/mesh/' + sha,
                    success: function(mesh) {
			selectionManager.deselectAll();

                        childNodes = selected.map(function(id) {
                            var node = geom_doc.findById(id);
                            geom_doc.remove(node);
                            return node;
                        });

                        boolNode = new GeomNode(geometry, childNodes);
                        boolNode.mesh = mesh;

                        geom_doc.add(boolNode);
			command_stack.commit();
                    },
		    error: function(jqXHR, textStatus, errorThrown) {
			command_stack.error(jqXHR.responseText);
		    }
                });
            },
            error: function(jqXHR, textStatus, errorThrown) {
                command_stack.error(jqXHR.responseText);
            }
        })};

    var undoFn = function() {
        geom_doc.remove(boolNode);
        childNodes.reverse().map(function(child) {
            geom_doc.add(child);
        });
	command_stack.success();
    }

    var redoFn = function() {
        childNodes.map(function(child) {
            geom_doc.remove(child);
        });
        geom_doc.add(boolNode);
	command_stack.success();
    }

    var cmd = new Command(doFn, undoFn, redoFn);
    command_stack.execute(cmd);
}

function copyNode(node, finishedFn) {

    var remaining = node.children.length;
    var copiedChildren = node.children.map(function(child) {
	return {};
    });

    var nodeCopyFn = function() {

	var geometry = JSON.parse(node.toShallowJson());
	geometry.children = copiedChildren.map(function(copiedChild) {
	    return copiedChild.path;
	});

	$.ajax({
	    type: 'POST',
	    url: '/geom/',
	    contentType: 'application/json',
	    data: JSON.stringify(geometry),
	    dataType: 'json',
	    success: function(nodeData) {
		var path = nodeData.path;
		var newNode = new GeomNode({
                    type : geometry.type,
                    path : path,
                    origin : geometry.origin,
                    parameters : geometry.parameters,
		    transforms : geometry.transforms
		}, copiedChildren);
		finishedFn(newNode);
	    }, 
	    error: function(jqXHR, textStatus, errorThrown) {
		error_response(jqXHR.responseText);
		command_stack.inProgressFailure();
	    }
	});
    };

    if (remaining == 0) {
	nodeCopyFn();
    } else {
	$.map(node.children, function(childNode, childIndex) {
	    var finishedFn = function(newChildNode) {
		copiedChildren.splice(childIndex - 1, 1, newChildNode);
		--remaining;
		if (remaining == 0) {
		    nodeCopyFn()
		}
	    };
	    copyNode(childNode, finishedFn);
	    ++childIndex;
	});
    }
}

function copy(selected) {
    if (selected.length !== 1)  {
        alert("must have 1 object selected");
        return;
    }

    var path = selected[0];
    var node = geom_doc.findByPath(path);
    
    var doFn = function() {

	copyNode(node, function(copiedNode) {
            id = idForGeomPath(copiedNode.path);
            $.ajax({
		type: 'GET',
		url: '/mesh/' + id,
		dataType: 'json',
		success: function(mesh) {
		    copiedNode.mesh = mesh;
                    selectionManager.deselectAll();
                    geom_doc.add(copiedNode);
                    command_stack.inProgressSuccess();
		},
		error: function(jqXHR, textStatus, errorThrown) {
		    error_response(jqXHR.responseText);
		}
            });
	});

    };
    var undoFn = function() {
        geom_doc.remove(copyNode);
	command_stack.inProgressSuccess();
    }
    var redoFn = function() {
        geom_doc.add(copyNode);
	command_stack.inProgressSuccess();
    }

    var cmd = new Command(doFn, undoFn, redoFn);
    command_stack.execute(cmd);
    
}

SS.save = function() {
    SS.spinner.show();
    
    $.ajax({
        type: 'PUT',
        url: '/' + SS.session.username + '/' + SS.session.design + '/refs/heads/master/',
        contentType: 'application/json',
        data: JSON.stringify(SS.session.commit),
        success: function(response) {
	    console.info('SAVE: ' + SS.session.commit);
	    var url = window.location.origin + window.location.pathname + '?ref=heads.master';
	    SS.spinner.hide();
        },
        error: function(jqXHR, textStatus, errorThrown) {
            error_response(jqXHR.responseText);
	    SS.spinner.hide();
        }
    });
}

SS.commit = function() {
    var rootSHAs = geom_doc.rootNodes.filter(function(x) {
        return !x.editing;
    }).map(function(x) {
        return x.sha;
    });
    var json = {'geoms' : rootSHAs,
		'parent' : SS.session.commit};
    $.ajax({
        type: 'POST',
        url: '/' + SS.session.username + '/' + SS.session.design + '/commit/',
        contentType: 'application/json',
        data: JSON.stringify(json),
        success: function(response) {
	    var commit = response.SHA;
	    console.info('COMMIT: ' + commit);

	    var url = window.location.origin + window.location.pathname + '?commit=' + commit;
	    history.pushState({commit: commit}, SS.session.design, url);
	    SS.session.commit = commit;
	    command_stack.success();
        },
        error: function(jqXHR, textStatus, errorThrown) {
            error_response(jqXHR.responseText);
        }
    });
}

window.onpopstate = function(event) {  
    var commit = $.getQueryParam("commit");
    SS.session.commit = commit;
    if (!command_stack.pop(commit)) {
	// No command stack available - load from disk
	SS.load_commit(commit);
    }
};

SS.load_commit = function(commit) {
    geom_doc.removeAll();
    SS.session.commit = commit;
    console.log('Load commit: ' + commit);
    SS.spinner.show();
    $.ajax({
        type: 'GET',
        url: '/' + SS.session.username + '/' + SS.session.design + '/commit/' + commit,
        dataType: 'json',
	success: function(design) { 
	    SS.spinner.hide();
	    console.log('Load design: ' + JSON.stringify(design));
	    var geoms = design.geoms;
	    geoms.map(function(geom) {
		SS.load_geom(geom);
	    });
			 
	},
	error: function(jqXHR, textStatus, errorThrown) {
	    SS.spinner.hide();
            error_response(jqXHR.responseText);
	}
    });
}


SS.load_geom = function(sha) {
    console.info('Loading geom: ' + sha);
    SS.spinner.show();

    $.ajax({
	type: 'GET',
	url: '/' + SS.session.username + '/' + SS.session.design + '/geom/' + sha,
	dataType: 'json',
	success: function(geomJson) {
	    geomJson.sha = sha;
	    var newNode = new GeomNode(geomJson);
	    $.ajax({
		type: 'GET',
		url: '/' + SS.session.username + '/' + SS.session.design + '/mesh/' + sha,
		success: function(mesh) {
		    newNode.mesh = mesh;
		    geom_doc.add(newNode);
		    SS.spinner.hide();
		},
		error: function(jqXHR, textStatus, errorThrown) {
		    error_response(jqXHR.responseText);
		    SS.spinner.hide();
		}
	    });
	    
	},
	error: function(jqXHR, textStatus, errorThrown) {
	    error_response(jqXHR.responseText);
	    SS.spinner.hide();
	}
    });
}
    
