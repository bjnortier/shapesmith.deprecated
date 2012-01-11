function renderTransform(geomNode, transformIndex) {
    var transform = geomNode.transforms[transformIndex];
    
    // Origin & Orientation
    var originTable = null;
    if (transform.origin) {
	var originArr = ['x', 'y', 'z'].map(function(key) {
	    return {key: key, 
		    value: transform.origin[key], 
		    'edit-class': 'edit-transform target-' + geomNode.id + '-' + transformIndex,
		    editing: transform.editing}
	});
	var originTemplate = '<table>{{#originArr}}<tr {{#editing}}class="field"{{/editing}}><td>{{key}}</td><td>{{^editing}}<span class="{{edit-class}}">{{value}}</span>{{/editing}}{{#editing}}<input id="{{key}}" type="text" value="{{value}}"/>{{/editing}}</td></tr>{{/originArr}}</table>';
	var originTable = $.mustache(originTemplate, {originArr : originArr});
    }

    // Params
    var paramsArr = [];
    for (key in transform.parameters) {
        paramsArr.push({key: key,
                        value: transform.parameters[key],
                        'edit-class': 'edit-transform target-' + geomNode.id + '-' + transformIndex,
                        editing: transform.editing
                       });
    }
    var parametersTemplate = '<table>{{#paramsArr}}<tr {{#editing}}class="field"{{/editing}}><td>{{key}}</td><td>{{^editing}}<span class="{{edit-class}}">{{value}}</span>{{/editing}}{{#editing}}<input id="{{key}}" type="text" value="{{value}}"/>{{/editing}}</td></tr>{{/paramsArr}}</table>';
    var paramsTable = $.mustache(parametersTemplate, {paramsArr : paramsArr});


    var template = '<table><tr><td>{{type}}{{^editing}}<img class="{{delete-class}}" src="/static/images/delete_button.png" alt="delete"/>{{/editing}}</td></tr><tr><td>{{{originTable}}}</td></tr><tr><td>       {{{paramsTable}}}</td></tr>{{#editing}}<tr><td><input id="modal-ok" type="submit" value="Ok"/><input id="modal-cancel" type="submit" value="Cancel"/></td></tr>{{/editing}}</table>';

    var view = {
        type: transform.type,
	editing: transform.editing,
        'delete-class': 'delete-transform target-' + geomNode.id + '-' + transformIndex,
	originTable: originTable,
	paramsTable: paramsTable};
    var transformTable = $.mustache(template, view);
    return transformTable;
}


function renderNode(geomNode) {

    // Origin & Orientation
    var originTable = null;
    if (geomNode.origin) {
	var originArr = ['x', 'y', 'z'].map(function(key) {
	    return {key: key, 
		    value: geomNode.origin[key], 
		    clazz: 'edit-geom target-' + geomNode.id,
		    editing: geomNode.editing}
	});
	var originTemplate = '<table>{{#originArr}}<tr {{#editing}}class="field"{{/editing}}><td>{{key}}</td><td>{{^editing}}<span class="{{clazz}}">{{value}}</span>{{/editing}}{{#editing}}<input id="{{key}}" type="text" value="{{value}}"/>{{/editing}}</td></tr>{{/originArr}}</table>';
	var originTable = $.mustache(originTemplate, {originArr : originArr});
    }

    // Params
    var paramsArr = [];
    if (!geomNode.isBoolean()) {

        for (key in geomNode.parameters) {

            paramsArr.push({key: key,
                            value: geomNode.parameters[key],
                            clazz: 'edit-geom target-' + geomNode.id,
                            editing: geomNode.editing
                           });
        }
    }
    var parametersTemplate = '<table>{{#paramsArr}}<tr {{#editing}}class="field"{{/editing}}><td>{{key}}</td><td>{{^editing}}<span class="{{clazz}}">{{value}}</span>{{/editing}}{{#editing}}<input id="{{key}}" type="text" value="{{value}}"/>{{/editing}}</td></tr>{{/paramsArr}}</table>';
    var paramsTable = $.mustache(parametersTemplate, {paramsArr : paramsArr});

    // Transforms
    var transformRows = []
    for (var i in geomNode.transforms) {
        transformRows.push(renderTransform(geomNode, i));
    };

    // Children
    var childTables = geomNode.children.map(renderNode);
    
    var childTemplate = '<table id="{{id}}"><tr><td><img class="show-hide-siblings siblings-showing" src="/static/images/arrow_showing.png"></img>{{^editing}}<span class="{{clazz}}">{{type}}</span>{{/editing}}{{#editing}}{{type}}{{/editing}}</td></tr><tr><td>{{{originTable}}}</td></tr><tr><td>{{{paramsTable}}}</td></tr>{{#editing}}<tr><td><input id="modal-ok" type="submit" value="Ok"/><input id="modal-cancel" type="submit" value="Cancel"/></td></tr>{{/editing}}{{#transformRows}}<tr><td>{{{.}}}</tr></td>{{/transformRows}}{{#children}}<tr><td>{{{.}}}</td></td>{{/children}}</table>';

    var clazz = geom_doc.isRoot(geomNode) ? 
	'select-geom target-' + geomNode.id : 
	'target-' + geomNode.id;
	
    var view = {type: geomNode.type,
                editing: geomNode.editing,
		id: geomNode.id,
		originTable: originTable,
                paramsTable: paramsTable,
                transformRows: transformRows,
                clazz: clazz,
                children: childTables
               };
    var nodeTableContents = $.mustache(childTemplate, view);
    return nodeTableContents;
}


function TreeView() {

    var that = this;
    this.domNodeLookup = {};

    this.addEvents = function(precursor, geomNode) {

        var hideNode = function(nodeElement) {
            nodeElement.attr('src', '/static/images/arrow_hidden.png');
            nodeElement.removeClass('siblings-showing');
            var otherRows = nodeElement.parent().parent().siblings();
            otherRows.hide();
        }

        var showNode = function(nodeElement) {
            nodeElement.attr('src', '/static/images/arrow_showing.png');
            nodeElement.addClass('siblings-showing');
            var otherRows = nodeElement.parent().parent().siblings();
            otherRows.show();
        }
        
        var toggleShowHide = function(nodeElement) {
            if ($(nodeElement).hasClass('siblings-showing')) {
                hideNode($(nodeElement));
            } else {
                showNode($(nodeElement));
            }
        }

	var transformBeingEdited; 
	for (var i in geomNode.transforms) {
	    if (geomNode.transforms[i].editing) {
		transformBeingEdited = geomNode.transforms[i];
	    }
	}
	
	var okGeomFunction = function() {
	    if (precursor) {
		for (key in geomNode.origin) {
                    geomNode.origin[key] = parseFloat($('#' + key).val());
		}
		for (key in geomNode.parameters) {
                    geomNode.parameters[key] = parseFloat($('#' + key).val());
		}
		return update_geom_command(precursor, geomNode, geomNode);
            } else {
		var origin = {};
		for (key in geomNode.origin) {
                    origin[key] = parseFloat($('#' + key).val());
		}
		var parameters = {};
		for (key in geomNode.parameters) {
                    parameters[key] = parseFloat($('#' + key).val());
		}
		return create_geom_command(geomNode, {type: geomNode.type,
						      origin: origin,
                                                      parameters: parameters});
            }
	}

	var okTransformFn = function() {
	    for (key in transformBeingEdited.origin) {
		transformBeingEdited.origin[key] = parseFloat($('#' + key).val());
	    }
	    for (key in transformBeingEdited.parameters) {
		transformBeingEdited.parameters[key] = parseFloat($('#' + key).val());
	    }
	    //geom_doc.replace(geomNode, precursor);
	    return update_geom_command(precursor, geomNode, geomNode);
	}

        if (geomNode.editing || transformBeingEdited) {
	    
            $('#modal-ok').click(function() {

                var cmd;
		if (geomNode.editing) {
                    cmd = okGeomFunction();
		} else {
		    cmd = okTransformFn();
		}
                command_stack.execute(cmd);
            });

	    var cancelFunction = function() {
		if (precursor) {
		    geom_doc.replace(geomNode, precursor);
                } else {
                    geom_doc.remove(geomNode);
                }
	    }

	    $(document).bind('keyup.editing', function(e) {
		if (e.keyCode == 27) { 
		    cancelFunction();
		}
	    });

            $('#modal-cancel').click(function() {
                cancelFunction();
            });
        } 

        // If neither the geomnode nor any of it's transforms are in editing, hide
        var anyTransformsEditing = false;
        for (var i in geomNode.transforms) {
            if (geomNode.transforms[i].editing) {
                anyTransformsEditing = true;
                break;
            }
        }
        if (!(geomNode.editing || anyTransformsEditing)) {
            hideNode($('#' + geomNode.id + ' .show-hide-siblings'));
        }

        $("#geom-model-doc tr:nth-child(even)").addClass("even");
        $("#geom-model-doc tr:nth-child(odd)").addClass("odd");

        $('.select-geom').click(function(event) {
            var id;
            var pattern = /^target-(.*)$/;
            var classes = $(this).attr('class').split(' ');
            for (var i in classes) {
                var match = classes[i].match(pattern);
                if (match) {
                    id = match[1];
                }
            }
            if (!id) {
                throw Error('id for editing could not be determined');
            }
	    selectionManager.pick(id);
        });

        // Edit geom
        $('.edit-geom').dblclick(function() { 
	    selectionManager.deselectAll();

            var id;
            var pattern = /^target-(.*)$/;
            var classes = $(this).attr('class').split(' ');
            for (var i in classes) {
                var match = classes[i].match(pattern);
                if (match) {
                    id = match[1];
                }
            }
            if (!id) {
                throw Error('id for editing could not be determined');
            }
	    that.edit(id);
        });
	

        // Edit transform
        $('.edit-transform').dblclick(function() { 
	    selectionManager.deselectAll();
            var id;
            var transformIndex;
            var pattern = /^target-(.*)-(.*)$/;
            var classes = $(this).attr('class').split(' ');
            for (var i in classes) {
                var match = classes[i].match(pattern);
                if (match) {
                    id = match[1];
                    transformIndex = match[2];
                }
            }
            var geomNode = geom_doc.findById(id);
            var editingNode = geomNode.editableCopy();
            editingNode.transforms[transformIndex].editing = true;
            geom_doc.replace(geomNode, editingNode);
        });

        $('.delete-transform').click(function() {
            var id;
            var transformIndex;
            var pattern = /^target-(.*)-(.*)$/;
            var classes = $(this).attr('class').split(' ');
            for (var i in classes) {
                var match = classes[i].match(pattern);
                if (match) {
                    id = match[1];
                    transformIndex = match[2];
                }
            }
            var geomNode = geom_doc.findById(id);
            var editingNode = geomNode.editableCopy();
            editingNode.transforms.splice(transformIndex, 1);
            var cmd = update_geom_command(geomNode, geomNode, editingNode);
            command_stack.execute(cmd);
        });

        // Show/Hide
        $('#' + geomNode.id + ' .show-hide-siblings').click(function() {
            toggleShowHide(this);
            return false;
        });
        
    }

    this.edit = function(id) {
	selectionManager.deselectAll();

	var geomNode = geom_doc.findById(id);
	if (!geomNode.isBoolean()) {
            var editingNode = geomNode.editableCopy();
            editingNode.editing = true;
            geom_doc.replace(geomNode, editingNode);
	    
	    if (SS.constructors[geomNode.type]) {
		SS.constructors[geomNode.type]().edit();
	    }
	}
    }
				 

    this.geomDocUpdated = function(event) {

	$(document).unbind('keyup.editing');

        if (event.add) {
            var geomNode = event.add;

            var nodeTable = renderNode(geomNode);

            this.domNodeLookup[geomNode] = nodeTable;
            $('#geom-model-doc').prepend(nodeTable);
            this.addEvents(null, geomNode);
        }

        if (event.remove) {
	    // Preview model is removed on cancel
	    SS.constructors.active && SS.constructors.active.dispose();

            var geomNode = event.remove;
            $('#' + geomNode.id).remove();
            delete this.domNodeLookup[geomNode];
        }

        if (event.replace) {
	    // Preview model is replaced with real model on success
	    SS.constructors.active && SS.constructors.active.dispose();

            var original = event.replace.original;
            var replacement = event.replace.replacement;
            var nodeTable = renderNode(replacement);
            $('#' + original.id).replaceWith(nodeTable);
            this.addEvents(original, replacement);
        }
    }

    this.selectionUpdated = function(event) {
        if (event.deselected) {
            var deselected = event.deselected;
            for (var i in deselected) {
                var id = deselected[i];
                $('#' + id + ' > tbody > tr:nth-child(1)').removeClass('selected');
            }
        }
        if (event.selected) {
            var selected = event.selected;
            for (var i in selected) {
                var id = selected[i];
                $('#' + id + ' > tbody > tr:nth-child(1)').addClass('selected');
            }
        }

    }
}




