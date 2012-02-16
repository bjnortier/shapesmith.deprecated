function Action(label, iconPath, fn) {
    this.label = label;
    this.iconPath = iconPath;
    this.fn = fn;

    this.render = function(toolbar) {

        var imgId = "action_" + label.toLowerCase().replace(/ /g, '_');
        toolbar.append('<img id="' + imgId + '" src="' + this.iconPath + '" title="' + label + '"/>');
        
        // Because 'this' is the HTML element inside the function below,
        // we have to use a reference
        var fn = this.fn;
	var ref = '#' + imgId;
        jQuery(ref).mouseenter(function() {
	    jQuery(ref).css('background-color', '#444');
	});
        jQuery(ref).mouseleave(function() {
	    jQuery(ref).css('background-color', '');
	});
        jQuery("#" + imgId).mouseup(function(event) {
	    // TODO: Move this to the popupmenu
	    var selected = selectionManager.getSelected();
	    SS.sceneView.popupMenu.disposeIfShowing();
	    fn(selected);
            event.stopPropagation();
        });
    }
}

function delete_geom(selected) {
    if (selected.length == 0)  {
        alert("please select at least one object");
        return;
    }
    var nodes = selected.map(function(id) {
	return geom_doc.findById(id);
    });

    var doFn = function() {
	for (var i in nodes) {
            geom_doc.remove(nodes[i]);
	}
	command_stack.commit();
    }

    var undoFn = function() {
	for (var i in nodes) {
            geom_doc.add(nodes[i]);
	}
	command_stack.success();
    }

    var redoFn = function() {
	for (var i in nodes) {
            geom_doc.remove(nodes[i]);
	}
	command_stack.success();
    }

    var cmd = new Command(doFn, undoFn, redoFn);
    command_stack.execute(cmd);

}


function create_primitive(type) {

    var geometryParams = {}, geomNode;
    var jsonSchema = SS.schemas[type];

    var createParams = function(key, context, schema) {
        if (schema.type !== 'array') {
            context[key] = null;
        } else {
            
            // Only support fixed length at the moment
            if (schema.minItems !== schema.minItems) {
                throw 'only fixed lenght arrays supported';
            }
            context[key] = [];
            for (var i = 0; i < schema.maxItems; ++i) {
                var params = {};
                for (subKey in schema.items.properties) {
                    createParams(subKey, params, schema.items.properties[subKey]);
                }
                context[key].push(params);
            }
        }
    }

    for (var key in jsonSchema.properties.parameters.properties) {
        createParams(key, geometryParams, jsonSchema.properties.parameters.properties[key]);
    }

    var lastMousePosition = SS.sceneView.workplane.getLastMousePosition();
    geomNode = new GeomNode({
        type: type,
        editing: true,
	origin: {x: lastMousePosition.x, y: lastMousePosition.y, z: 0},
        parameters: geometryParams});
    geom_doc.add(geomNode);
    return geomNode;
}

function create_modifier(selected, type) {
    if (selected.length != 1)  {
        alert('Only a single object is supported!');
        return;
    }
    var id = selected[0];
    var original = geom_doc.findById(id);

    var jsonSchema = SS.schemas[type];

    var geometryParams = {};
    for (var key in jsonSchema.properties.parameters.properties) {
        geometryParams[key] = null;
    }
    modifierNode = new GeomNode({
        type: type,
        editing: true,
	origin: {x: 0, y: 0, z: 0},
        parameters: geometryParams
    }, [original]);
                                
    geom_doc.replace(original, modifierNode);

    return modifierNode;
}

function create_transform(selected, type) {
    if (selected.length != 1)  {
        alert('no object selected!');
        return;
    }

    var jsonSchema = SS.schemas[type];
    var transformParams = {};
    for (var key in jsonSchema.properties.parameters.properties) {
        transformParams[key] = null;
    }
    
    var id = selected[0];
    
    var original = geom_doc.findById(id);
    var replacement = original.editableCopy();
    var origin = {x: 0, y: 0, z: 0};
    if (((type === 'translate') || (type === 'scale')) && (original.origin)) {
	origin = {x: original.origin.x,
                  y: original.origin.y,
                  z: original.origin.z};
    }
    
    var transform = new Transform({
        type: type,
        editing: true,
	origin: origin,
        parameters: transformParams
    });
    replacement.transforms.push(transform);
    geom_doc.replace(original, replacement);
    return {geomNode: replacement, transform: transform};
}


$(document).ready(function() {

    // Edit
    new Action('Delete', '/static/images/trash.png', 
               function(selected) { 
		   delete_geom(selected); 
	       }).render($('#edit'));
    new Action('Copy', '/static/images/copy.png', 
               function(selected) { 
		   copy(selected)
	       }).render($('#edit'));


    //
    // Primitives
    //
    new Action('Cuboid', '/static/images/cuboid.png', 
               function() { 
		   new SS.CuboidCreator({editingNode: create_primitive('cuboid')});
	       }).render($('#3Dprimitives'));
    new Action('Sphere', '/static/images/sphere.png', 
               function() { 
                   new SS.SphereCreator({editingNode: create_primitive('sphere')});
	       }).render($('#3Dprimitives'));
    new Action('Cylinder', '/static/images/cylinder.png', 
               function() { 
		   new SS.CylinderCreator({editingNode: create_primitive('cylinder')}); 
	       }).render($('#3Dprimitives'));
    new Action('Cone', '/static/images/cone.png', 
               function() { 
		   new SS.ConeCreator({editingNode: create_primitive('cone')});
	       }).render($('#3Dprimitives'));
    new Action('Wedge', '/static/images/wedge.png', 
               function() { 
		   new SS.WedgeCreator({editingNode: create_primitive('wedge')});
	       }).render($('#3Dprimitives'));
    new Action('Torus', '/static/images/torus.png', 
               function() { 
		   new SS.TorusCreator({editingNode: create_primitive('torus')});
	       }).render($('#3Dprimitives'));

    new Action('Ellipse 2D', '/static/images/ellipse2d.png', 
               function() { 
                   new SS.Ellipse2DCreator({editingNode: create_primitive('ellipse2d')});
	       }).render($('#2Dprimitives'));
    new Action('Rectangle 2D', '/static/images/rectangle2d.png', 
               function() { 
                   new SS.Rectangle2DCreator({editingNode: create_primitive('rectangle2d')});
	       }).render($('#2Dprimitives'));
    new Action('Triangle 2D', '/static/images/triangle2d.png', 
               function() { 
                   new SS.Triangle2DCreator({editingNode: create_primitive('triangle2d')});
	       }).render($('#2Dprimitives'));
    new Action('Text 2D', '/static/images/text2d.png', 
               function() { 
                   SS.constructors.createText2d(create_primitive('text2d'));
	       }).render($('#2Dprimitives'));

    /*new Action('Ellipse 1D', '/static/images/ellipse1d.png', 
               function() { 
                   SS.constructors.createEllipse1d(create_primitive('ellipse1d',  ['r1', 'r2']));
	       }).render($('#1Dprimitives'));*/
    
    //
    // Booleans
    //
    new Action('Union', '/static/images/union.png', 
               function(selected) { boolean(selected, 'union'); }).render($('#boolean'));
    new Action('Subtract', '/static/images/diff.png', 
               function(selected) { boolean(selected, 'subtract'); }).render($('#boolean'));
    new Action('Intersect', '/static/images/intersect.png', 
               function(selected) { boolean(selected, 'intersect'); }).render($('#boolean'));
    new Action('Explode', '/static/images/explode.png', 
           function(selected) { explode(selected, 'intersect'); }).render($('#edit'));

    //
    // Modifiers
    //
    new Action('Prism', '/static/images/prism.png', 
               function(selected) { 
                   SS.constructors.createPrism(create_modifier(selected, 'prism'));
               }).render($('#modifiers'));
    
    /*
     * Transformations
     */
    /*new Action('Mirror', '/static/images/mirror.png', 
               function(selected) { 
		   SS.constructors.createMirror(create_transform(selected, 'mirror'));
	       }).render($('#transforms'));*/
    
});


$('#action-save').click(function() {
    SS.save();
});

$('#action-export-stl').click(function() {
    window.location = '/' + SS.session.username + '/' + SS.session.design + '/stl/' + SS.session.commit + '/';
});

$('#action-export-thingiverse').click(function() {
    $('#black-overlay').show();
    $('#thingiverse-export').show();
});

$('form#thingiverse-export').submit(function() {
    $('input[type=submit]', this).attr('disabled', 'disabled');
});

$('#thingiverse-export input.ok').click(function() {
    $('#thingiverse-export input.ok').attr('disabled', 'disabled');
    $.ajax({
        type: 'POST',
        url: '/' + SS.session.username + '/' + SS.session.design + '/stl/publish/' + SS.session.commit, 
	data: JSON.stringify({}),
	success: function(design) { 
	    var publicSTLUrl = SS.session.host + '/' + SS.session.username + '/' + SS.session.design + '/stl/' + SS.session.commit + '.stl';
	    $('#public_stl_location_b64').val(window.btoa(publicSTLUrl));
	    $('#thingiyverse-export-form').submit();
	    $('#black-overlay').hide();
	    $('#thingiverse-export').hide();
	    $('#thingiverse-export input.ok').removeAttr('disabled');
	},
	error: function(jqXHR, textStatus, errorThrown) {
	    alert(jqXHR.responseText);
	    $('#thingiverse-export input.ok').removeAttr('disabled');
	}
    });
    return false;
});

$('#thingiverse-export input.cancel').click(function() {
    $('#black-overlay').hide();
    $('#thingiverse-export').hide();
    return false;
});
