var SS = SS || {};

function update_geom_command(originalNode, nodeInDoc, replacement) {

    var chainedPostFn = function(index, fromChain, toChain) {

        var nextFrom = fromChain[index];
        var nextTo = toChain[index];

        if (nextTo) {
	    var isLast = !(index + 1 < fromChain.length);
	    var createUrl = isLast ? 
		'/' + SS.session.username + '/' + SS.session.design + '/geom?mesh=true' :
		'/' + SS.session.username + '/' + SS.session.design + '/geom/';
            $.ajax({
		type: 'POST',
		url: createUrl,
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
			var foundChildIndex = -1;
			var fromParent = fromChain[index+1];
			for (var childIndex in fromParent.children) {
			    if ((fromParent.children[childIndex] == nextFrom) ||
				(fromParent.children[childIndex] == replacement)) {
				foundChildIndex = childIndex;
			    }
			}
			if (foundChildIndex === -1) {
			    throw('child not found');
			}
			toChain[index+1].children.splice(foundChildIndex, 1, nextTo);
		    }

                    if (index + 1 < fromChain.length) {
                        chainedPostFn(index + 1, fromChain, toChain);
                    } else {
                        // No more -> update the root node
                        nextTo.mesh = result.mesh;

			// If the 'to' node is the preview node, or node that 
			// was edited, that one is to be replaced, not the original
			// node
			if (index == 0) {
			    geom_doc.replace(nodeInDoc, nextTo);
			} else {
                            geom_doc.replace(nextFrom, nextTo);
			}
			command_stack.commit();
                    }
                },
		error: function(jqXHR, textStatus, errorThrown) {
		    command_stack.error(jqXHR.responseText);
                }
            });
        }
    }

    // toNode is the preview node so will be in the geom doc
    var fromAncestors = geom_doc.ancestors(nodeInDoc);
    var toAncestors = fromAncestors.map(function(ancestor) {
        return ancestor.editableCopy();
    });

    var fromChain = [originalNode].concat(fromAncestors);
    var toChain = [replacement.editableCopy()].concat(toAncestors);

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
            url: '/' + SS.session.username + '/' + SS.session.design + '/geom?mesh=true',
            contentType: 'application/json',
            data: JSON.stringify(geometry),
	    dataType: 'json',
            success: function(result) {
   		geometry.sha = result.SHA;
                geomNode = new GeomNode(geometry);
                geomNode.mesh = result.mesh;

                if (prototype) {
                    geom_doc.replace(prototype, geomNode);
                } else {
                    geom_doc.add(geomNode);
                }
                command_stack.commit();
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

function explode(selected, type) {
    if (selected.length !== 1)  {
        alert("must have 1 object selected");
        return;
    }

    var boolNode = geom_doc.findById(selected[0]);
    var childNodes = boolNode.children;


    if (childNodes.length === 0) {
        alert("You can only explode an item with children (e.g. booleans)");
        return;
    }

    var doFn = function() {
        var childrenToFetchMeshesFor = childNodes.filter(function(childNode) {
            return !childNode.mesh;
        });

        var replaceInGeomDoc = function() {
            geom_doc.remove(boolNode);
            childNodes.map(function(childNode) {
                geom_doc.add(childNode);
            });
            command_stack.commit();
        };

        if (childrenToFetchMeshesFor.length == 0) {
            replaceInGeomDoc();
        } else {
            var copies = childrenToFetchMeshesFor.slice(0);
            copies.map(function(childNode) {
                $.ajax({
	            type: 'GET',
	            url: '/' + SS.session.username + '/' + SS.session.design + '/mesh/' + childNode.sha,
	            success: function(mesh) {
		        childNode.mesh = mesh;
                        childrenToFetchMeshesFor.splice(childrenToFetchMeshesFor.indexOf(childNode), 1);
                        if (childrenToFetchMeshesFor.length == 0) {
                            replaceInGeomDoc();
                        }
                    },
	            error: function(jqXHR, textStatus, errorThrown) {
		        command_stack.error(jqXHR.responseText);
	            }
	        });
            });
        }

    }

    var undoFn = function() {
        childNodes.map(function(child) {
            geom_doc.remove(child);
        });
        geom_doc.add(boolNode);
	command_stack.success();
    }

    var redoFn = function() {
        geom_doc.remove(boolNode);
        childNodes.reverse().map(function(child) {
            geom_doc.add(child);
        });
	command_stack.success();
    }

    var cmd = new Command(doFn, undoFn, redoFn);
    command_stack.execute(cmd);
}



function boolean(selected, type) {
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
	    url: '/' + SS.session.username + '/' + SS.session.design + '/geom?mesh=true',
            contentType: "application/json",
            data: JSON.stringify(geometry),
            success: function(result) {
		var sha = result.SHA;
		geometry.sha = sha;
		
                childNodes = selected.map(function(id) {
                    var node = geom_doc.findById(id);
                    geom_doc.remove(node);
                    return node;
                });

                boolNode = new GeomNode(geometry, childNodes);
                boolNode.mesh = result.mesh;
		
                geom_doc.add(boolNode);
		command_stack.commit();
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
    copyNode(node);
}

function importJSON(json) {
    var rootGeometries = JSON.parse(json);
    var rootNodes = [];
    var geomNodes = {};

    // Find the next geometry in the tree to create. It will be the first
    // geometry found that is not a SHA (a string)
    var createNext = function(geometries) {
        for (var i = 0; i < geometries.length; ++i) {
            if (geometries[i].children) {
                var found = createNext(geometries[i].children);
                if (found) {
                    return true;
                }
            }
            if (typeof(geometries[i]) !== 'string') {
                var url = (geometries === rootGeometries) ? 
                    '/' + SS.session.username + '/' + SS.session.design + '/geom?mesh=true' :
                    '/' + SS.session.username + '/' + SS.session.design + '/geom';
                $.ajax({
                    type: 'POST',
                    url: url,
                    contentType: 'application/json',
                    data: JSON.stringify(geometries[i]),
	            dataType: 'json',
                    success: function(result) {
                        geometries[i].sha = result.SHA;
                        
                        var children = geometries[i].children || [];
                        var geomNode = new GeomNode(geometries[i], children.map(function(sha) {
                            return geomNodes[sha];
                        }));
                        if (result.mesh) {
                            geomNode.mesh = result.mesh;
                        }
                        geomNodes[result.SHA] = geomNode;
                        
                        geometries[i] = result.SHA;
                        createNext(rootGeometries);
                    },
                    error: function(jqXHR, textStatus, errorThrown) {
                        command_stack.error(jqXHR.responseText);
                    }
                });
                return true;
            }
        }

        // No more to create, add them to the geom document
        if (geometries === rootGeometries) {
            geometries.map(function(sha) {
                var geomNode = geomNodes[sha];
                rootNodes.push(geomNode);
                geom_doc.add(geomNode);
            });
            command_stack.commit();
        }

        return false;
    }

    var doFn = function() {
        createNext(rootGeometries);
    }

    var undoFn = function() {
        rootNodes.map(function(node) {
            geom_doc.remove(node);
        });
	command_stack.success();
    };
    var redoFn = function() {
        rootNodes.map(function(node) {
            geom_doc.add(node);
        });
	command_stack.success();
    }
    
    command_stack.execute(new Command(doFn, undoFn, redoFn));
}



function copyNode(node) {
    var newNode;
    
    var doFn = function() {

	var copyWithChildren = function(node) {
	    var copiedChildren = node.children.map(function(child) {
		return new GeomNode(child);
	    });
	    return new GeomNode(node, copiedChildren);
	};
	newNode = copyWithChildren(node);

        if (newNode.mesh) {
	    geom_doc.add(newNode);
	    command_stack.commit();
        } else {
             $.ajax({
	         type: 'GET',
	         url: '/' + SS.session.username + '/' + SS.session.design + '/mesh/' + newNode.sha,
	         success: function(mesh) {
		     newNode.mesh = mesh;
	             geom_doc.add(newNode);
	             command_stack.commit();
                 },
	         error: function(jqXHR, textStatus, errorThrown) {
		     command_stack.error(jqXHR.responseText);
	         }
	     });
        }
	
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
	    SS.renderInfoMessage('Saved');
	    SS.spinner.hide();
        },
        error: function(jqXHR, textStatus, errorThrown) {
	    SS.spinner.hide();
	    SS.renderErrorMessage(jqXHR.responseText);
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

	    var url = window.location.pathname + '?commit=' + commit;
	    history.pushState({commit: commit}, SS.session.design, url);
	    SS.session.commit = commit;
	    command_stack.success();
        },
        error: function(jqXHR, textStatus, errorThrown) {
	    command_stack.error(jqXHR.responseText);
        }
    });
}

/*
 * This is a pain. Chrome and Firefox have differrent behaviour for window.onpopstate().
 * Firefox will NOT generate the event when there is no session state available (e.g.
 * on a new page load). Chrome will generate and event, but Firefox will not. Hence
 * the need to $(document).ready() and the workaround variable.
 */

SS.loadCommitFromStateOrParam = function(state) {
    var commit = (state && state.commit) || $.getQueryParam("commit");
    SS.session.commit = commit;
    if (!command_stack.pop(commit)) {
	// No command stack available - load from disk
	SS.load_commit(commit);
    }
};

window.onpopstate = function(event) { 
    SS.loadCommitFromStateOrParam(event.state);
};

$(document).ready(function() {
    // Only necessary in FF since. See above comment.
    if (navigator.userAgent.indexOf("Firefox") != -1) {
	SS.loadCommitFromStateOrParam(undefined);
    }
});


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
	    SS.renderErrorMessage(jqXHR.responseText);
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
		    SS.spinner.hide();
		    SS.renderErrorMessage(jqXHR.responseText);
		}
	    });
	    
	},
	error: function(jqXHR, textStatus, errorThrown) {
	    SS.spinner.hide();
	    SS.renderErrorMessage(jqXHR.responseText);
	}
    });
}
    
