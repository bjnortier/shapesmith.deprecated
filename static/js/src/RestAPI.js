var SS = SS || {};

function update_geom_command(fromNode, toNode) {

    var chainedPostFn = function(index, fromChain, toChain) {

        var nextFrom = fromChain[index];
        var nextTo = toChain[index];

        if (nextTo) {
            $.ajax({
		type: 'POST',
		url: '/' + SS.session.username + '/' + SS.session.design + '/geom/',
                contentType: 'application/json',
                data: nextTo.toShallowJson(),
		dataType: 'json',
                success: function(result) {
		    var sha = result.SHA;
		    nextTo.setSHA(sha);
                    nextTo.editing = false;
                    for (var i in nextTo.transforms) {
                        nextTo.transforms[i].editing = false;
                    }

		    // Update the children of the next boolean node
		    // with the new SHA
		    if (index + 1 < fromChain.length) {
			var oldChildSHA = nextFrom.sha;
			var newChildSHA = nextTo.sha;
			var foundChildIndex = -1;
			for (var childIndex in fromChain[index+1].children) {
			    if (fromChain[index+1].children[childIndex].sha == oldChildSHA) {
				foundChildIndex = childIndex;
			    }
			}
			toChain[index+1].children.splice(foundChildIndex, 1, nextTo);
		    }

                    if (index + 1 < fromChain.length) {
                        chainedPostFn(index + 1, fromChain, toChain);
                    } else {
                        // No more -> update the root node
                        $.ajax({
                            type: 'GET',
			    url: '/' + SS.session.username + '/' + SS.session.design + '/mesh/' + sha,
                            success: function(mesh) {
                                nextTo.mesh = mesh;
				selectionManager.deselectAll();
                                geom_doc.replace(nextFrom, nextTo);
				command_stack.commit();
                            },
			    error: function(jqXHR, textStatus, errorThrown) {
				command_stack.error(jqXHR.responseText);
			    }

                        });
                    }
                },
		error: function(jqXHR, textStatus, errorThrown) {
		    command_stack.error(jqXHR.responseText);
                }
            });
        }
    }

    var fromAncestors = geom_doc.ancestors(fromNode);
    var toAncestors = fromAncestors.map(function(ancestor) {
        return ancestor.editableCopy();
    });

    var fromChain = [fromNode].concat(fromAncestors);
    var toChain = [toNode].concat(toAncestors);

    var doFn = function() {
        chainedPostFn(0, fromChain, toChain);
    };
    var undoFn = function() {
	geom_doc.replace(toChain[toChain.length - 1], fromChain[fromChain.length - 1]);
	command_stack.success();
    };
    var redoFn = function() {
	geom_doc.replace(fromChain[toChain.length - 1], toChain[fromChain.length - 1]);
	command_stack.success();
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
                $.ajax({
                    type: 'GET',
                    url: '/' + SS.session.username + '/' + SS.session.design + '/mesh/' + sha,
		    dataType: 'json',
                    success: function(mesh) {
			geometry.sha = sha;
                        geomNode = new GeomNode(geometry);
                        geomNode.mesh = mesh;

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
            alert("must have 2 or more objects selected!");
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

function copy(selected) {
    if (selected.length !== 1)  {
        alert("must have 1 object selected");
        return;
    }

    var id = selected[0];
    var node = geom_doc.findById(id);
    var newNode;
    
    var doFn = function() {

	var copyWithChildren = function(node) {
	    var copiedChildren = node.children.map(function(child) {
		return new GeomNode(child);
	    });
	    return new GeomNode(node, copiedChildren);
	};
	
	newNode = copyWithChildren(node);
	selectionManager.deselectAll();
	geom_doc.add(newNode);
	command_stack.commit();
    };
    var undoFn = function() {
        geom_doc.remove(newNode);
	command_stack.success();
    }
    var redoFn = function() {
        geom_doc.add(newNode);
	command_stack.success();
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
	url: '/' + SS.session.username + '/' + SS.session.design + '/geom/' + sha + '?recursive=true',
	dataType: 'json',
	success: function(json) {
	    var newNode = GeomNode.fromDeepJson(json);
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
    
