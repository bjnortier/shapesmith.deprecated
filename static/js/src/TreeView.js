function renderValue(clazz, key,  value, context, jsonSchema) {

    var item = jsonSchema.properties[key];
    if (item.type === 'array') {
        
        var elements = '';
        for (var k = 0; k < context[key].length; ++k) {

            var object = context[key][k];
            var subSchema = jsonSchema.properties[key].items;
            var items = [];
            for (var subKey in object) {
                var template = '<tr><td>{{label}}</td><td>' + renderValue(clazz, subKey, object[subKey], object[subKey], subSchema) + '</td></tr>';
                items.push($.mustache(template, {label: subKey, 
                                                 value: context[key][k][subKey],
                                                 key: subKey + '_' + k}));
            }

            var template = '<table><tr><td>{{index}}</td><td><table>{{#items}}{{{.}}}{{/items}}</table></td></tr></table>';
            var view = {
                index: k,
                items: items
            };
            var x = $.mustache(template, view);
            elements += x;
        }
        return elements;
    } else {
        return $.mustache('<span class="{{clazz}}">{{value}}</span>', 
                          {value: value,
                           clazz: clazz});
    }
    
}


function renderTransform(geomNode, transformIndex) {
    var transform = geomNode.transforms[transformIndex];
    var schema = SS.schemas[transform.type];
    
    // Origin & Orientation
    var originTable = null;
    if (transform.origin) {
        var originArr = [];
        for (key in transform.origin) {
            originArr.push({key: key, 
                            value: transform.origin[key], 
                            'edit-class': 'edit-transform target-' + geomNode.id + '-' + transformIndex,
                            editing: transform.editing
                           });
        }
        var originTemplate = '<table>{{#originArr}}<tr><td>{{key}}</td><td>{{^editing}}<span class="{{edit-class}}">{{value}}</span>{{/editing}}</td></tr>{{/originArr}}</table>';
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
    var parametersTemplate = '<table>{{#paramsArr}}<tr ><td>{{key}}</td><td>{{^editing}}<span class="{{edit-class}}">{{value}}</span>{{/editing}}</td></tr>{{/paramsArr}}</table>';
    var paramsTable = $.mustache(parametersTemplate, {paramsArr : paramsArr});

    var template = '{{#editing}}<div id="editing-area"></div>{{/editing}}{{^editing}}<table><tr><td><img class="show-hide-contents" src="/static/images/arrow_hidden.png"></img>{{type}}<img class="{{delete-class}}" src="/static/images/delete_button_gray_10.png" alt="delete"/></td></tr><tr style="{{contentsStyle}}"><td>{{{originTable}}}</td></tr><tr style="{{contentsStyle}}"><td>       {{{paramsTable}}}</td></tr></table>{{/editing}}';

    var view = {
        type: transform.type,
        editing: transform.editing,
        contentsStyle: (transform.editing ? '' : 'display:none;'),
        'delete-class': 'delete-transform target-' + geomNode.id + '-' + transformIndex,
        originTable: originTable,
        paramsTable: paramsTable};
    var transformTable = $.mustache(template, view);
    return transformTable;
}


function renderNode(geomNode) {

    var schema = SS.schemas[geomNode.type];

    // Origin & Orientation
    var originTable = null;
    if (schema.properties.origin) {
        var originArr = [];
        for (key in geomNode.origin) {
            originArr.push({key: key, 
                            value: geomNode.origin[key], 
                            clazz: 'edit-geom target-' + geomNode.id,
                            editing: geomNode.editing
                           });
        }
        var originTemplate = '<table>{{#originArr}}<tr {{#editing}}class="field"{{/editing}}><td>{{key}}</td><td>{{^editing}}<span class="{{clazz}}">{{value}}</span>{{/editing}}</td></tr>{{/originArr}}</table>';
        var originTable = $.mustache(originTemplate, {originArr : originArr});
    }

    // Params
    var paramsArr = [];
    if (schema.properties.parameters) {
        for (key in geomNode.parameters) {
            paramsArr.push({key: key,
                            element: renderValue('edit-geom target-' + geomNode.id, 
                                                 key, 
                                                 geomNode.parameters[key],
                                                 geomNode.parameters, 
                                                 schema.properties.parameters),
                            editing: geomNode.editing
                           });
        }
    }
    var parametersTemplate = '<table>{{#paramsArr}}<tr {{#editing}}class="field"{{/editing}}><td>{{key}}</td><td>{{^editing}}{{{element}}}{{/editing}}</td></tr>{{/paramsArr}}</table>';
    var paramsTable = $.mustache(parametersTemplate, {paramsArr : paramsArr});

    // Transforms
    var transformRows = []
    for (var i in geomNode.transforms) {
        transformRows.push(renderTransform(geomNode, i));
    };

    // Children
    var childTables = geomNode.children.map(renderNode);
    
    var childTemplate = '{{#editing}}<div id="{{id}}"><div id="editing-area"></div></div>{{/editing}}{{^editing}}<table id="{{id}}" class="geomnode-table"><tr><td><img class="show-hide-siblings siblings-showing" src="/static/images/arrow_showing.png"></img><span class="{{clazz}}">{{type}}</span><span class="node-actions">{{{copyHtml}}}}{{{deleteGeomHtml}}}{{{eyeHtml}}</span></td></tr><tr><td>{{{originTable}}}</td></tr><tr><td>{{{paramsTable}}}</td></tr>{{#transformRows}}<tr><td>{{{.}}}</tr></td>{{/transformRows}}{{#children}}<tr><td>{{{.}}}</td></td>{{/children}}</table>{{/editing}}';

    var clazz = geom_doc.isRoot(geomNode) ? 
        'select-geom target-' + geomNode.id : 
        'target-' + geomNode.id;

    var copyTemplate = '<img class="copy-geom {{clazz}}" src="/static/images/copy_10.png"/>'
    var copyHtml = $.mustache(copyTemplate, {clazz: 'target-' + geomNode.id});

    var eyeTemplate = '{{#isRoot}}<img class="toggle-geom-visibility {{clazz}} opaque" src="/static/images/eye.png"/> {{/isRoot}}';
    var eyeView = {
        isRoot: geom_doc.isRoot(geomNode),
        clazz:  'target-' + geomNode.id
    };
    var eyeHtml = $.mustache(eyeTemplate, eyeView);

    var deleteGeomTemplate = '{{#isRoot}}<img class="delete-geom {{delete-clazz}}" src="/static/images/delete_button_gray_10.png" alt="delete"/>{{/isRoot}}';
    var deleteGeomHtml = $.mustache(deleteGeomTemplate, 
                                    {isRoot: geom_doc.isRoot(geomNode),
                                     'delete-clazz': 'target-' + geomNode.id,
                                    });

        
    var view = {type: geomNode.type,
                editing: geomNode.editing,
                id: geomNode.id,
                copyHtml: copyHtml,
                eyeHtml: eyeHtml,
                deleteGeomHtml: deleteGeomHtml,
                originTable: originTable,
                paramsTable: paramsTable,
                transformRows: transformRows,
                clazz: clazz,
                children: childTables
               };
    var nodeTableContents = $.mustache(childTemplate, view);
    return nodeTableContents;

    //return SS.renderDisplayDOM(geomNode.type, SS.schemas[geomNode.type], geomNode);
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
            var updateParameters = function(key, parameters) {
                var schema = SS.schemas[geomNode.type];
                var itemSchema = schema.properties.parameters.properties[key];
                
                if (itemSchema.type === 'string') {
                    parameters[key] = $('#' + key).val();
                } else {
                    parameters[key] = parseFloat($('#' + key).val());
                }

            };
            
            if (precursor) {
                for (key in geomNode.origin) {
                    geomNode.origin[key] = parseFloat($('#' + key).val());
                }
                for (key in geomNode.parameters) {
                    updateParameters(key, geomNode.parameters);
                }
                return update_geom_command(precursor, geomNode, geomNode);
            } else {
                var origin = {};
                for (key in geomNode.origin) {
                    origin[key] = parseFloat($('#' + key).val());
                }
                var parameters = {};
                for (key in geomNode.parameters) {
                    updateParameters(key, parameters);
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

        $('.select-geom').unbind('click');
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
        $('.edit-geom').unbind('dblclick');
        $('.edit-geom').dblclick(function() { 
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

        // Copy geom
        $('.copy-geom').unbind('click');
        $('.copy-geom').click(function() { 
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
            var geomNode = geom_doc.findById(id);
            copyNode(geomNode);
        });
        

        // Edit transform
        $('.edit-transform').unbind('dblclick');
        $('.edit-transform').dblclick(function() { 
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

            var transform = editingNode.transforms[transformIndex];

            var constructor = SS.creators[transform.type];
            new constructor({originalNode: geomNode,
                             editingNode: editingNode, 
                             editingExisting: true,
                             transform: transform});
        });

        $('.delete-geom').unbind('click');
        $('.delete-geom').click(function() {
            var id;
            var pattern = /^target-(.*)$/;
            var classes = $(this).attr('class').split(' ');
            for (var i in classes) {
                var match = classes[i].match(pattern);
                if (match) {
                    id = match[1];
                }
            }
            var geomNode = geom_doc.findById(id);
            delete_geom_nodes([geomNode]);
        });

        $('.delete-transform').unbind('click');
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
        $('.show-hide-contents').unbind('click');
        $('.show-hide-contents').click(function() {
            var tbody = $(this).parent().parent().parent();
            if ($(this).hasClass('contents-showing')) {
                $(this).attr('src', '/static/images/arrow_hidden.png');
                $(this).removeClass('contents-showing');
                for (var i = 1; i < tbody.children().length; ++i) {
                    $(tbody.children()[i]).hide();
                }
            } else {
                $(this).attr('src', '/static/images/arrow_showing.png');
                $(this).addClass('contents-showing');
                for (var i = 1; i < tbody.children().length; ++i) {
                    $(tbody.children()[i]).show();
                }
            }
            return false;
        });

        $('.toggle-geom-visibility').unbind('click');
        $('.toggle-geom-visibility').click(function() {
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
            if ($(this).attr('class').indexOf('opaque') !== -1) {
                $(this).attr('src', '/static/images/eye_hidden.png');
                $(this).removeClass('opaque');
                $(this).addClass('hidden');
                SS.geomNodeRenderingManager.setHidden(id);
            } else if ($(this).attr('class').indexOf('hidden') !== -1) {
                $(this).attr('src', '/static/images/eye.png');
                $(this).removeClass('hidden');
                $(this).addClass('opaque');
                SS.geomNodeRenderingManager.setOpaque(id);

            }
        });

        // Show/Hide
        $('#' + geomNode.id + ' .show-hide-siblings').click(function() {
            toggleShowHide(this);
            return false;
        });
    }

    this.edit = function(id) {
        var geomNode = geom_doc.findById(id);
        if (SS.creators[geomNode.type]) {
            var editingNode = geomNode.editableCopy();
            editingNode.editing = true;
            selectionManager.deselectAll();
            geom_doc.replace(geomNode, editingNode);
            new SS.creators[geomNode.type]({editingNode: editingNode, originalNode: geomNode});
        }
    }
                                 

    this.geomDocAdd = function(geomNode) {
        var nodeTable = renderNode(geomNode);
        
        this.domNodeLookup[geomNode] = nodeTable;
        $('#geom-model-doc').prepend(nodeTable);
        this.addEvents(null, geomNode);
    }

    this.geomDocRemove = function(geomNode) {
        $(document).unbind('keyup.editing');

        // Preview model is removed on cancel
        SS.materials.active && SS.materials.disposeActive();

        $('#' + geomNode.id).remove();
        delete this.domNodeLookup[geomNode];
    }

    this.geomDocReplace = function(original, replacement) {
        $(document).unbind('keyup.editing');

        // Preview model is replaced with real model on success
        SS.materials.active && SS.materials.disposeActive();
        
        var nodeTable = renderNode(replacement);
        $('#' + original.id).replaceWith(nodeTable);
        this.addEvents(original, replacement);
    }

    this.deselected = function(deselected) {
        for (var i in deselected) {
            var id = deselected[i];
            $('#' + id + ' > tbody > tr:nth-child(1)').removeClass('selected');
        }
    }

    this.selected = function(selected) {
        for (var i in selected) {
            var id = selected[i];
            $('#' + id + ' > tbody > tr:nth-child(1)').addClass('selected');
        }
    }
    
    $('#advanced').change(function() {
        $('#geom-model-doc :visible').length > 0 ? 
            $('#geom-model-doc').hide() :
            $('#geom-model-doc').show();
        
    });
        

    geom_doc.on('add', this.geomDocAdd, this);
    geom_doc.on('remove', this.geomDocRemove, this);
    geom_doc.on('replace', this.geomDocReplace, this);

    selectionManager.on('selected', this.selected, this);
    selectionManager.on('deselected', this.deselected, this);
}




