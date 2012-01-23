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
	    sceneView.popupMenu.disposeIfShowing();
	    sceneView.onMouseUp(event);
	    fn(selected);
            
        });
    }
}

function delete_geom(selected) {
    selectionManager.deselectAll();

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


function create_primitive(type, keys) {

    var geometryParams = {}, geomNode;
    for (var i in keys) {
        geometryParams[keys[i]] = null;
    }
    geomNode = new GeomNode({
        type: type,
        editing: true,
	origin: {x: 0, y: 0, z: 0},
        parameters: geometryParams});
    geom_doc.add(geomNode);
    return geomNode;
}

function create_transform(selected, type, keys) {
    if (selected.length != 1)  {
        alert('no object selected!');
        return;
    }
    var transformParams = {};
    for (var i in keys) {
        transformParams[keys[i]] = null;
    }
    
    var id = selected[0];
    
    var original = geom_doc.findById(id);
    var replacement = original.editableCopy();
    var origin = {x: 0, y: 0, z: 0};
    if (((type === 'translate') || (type === 'scale')) && (original.origin)) {
	origin = original.origin;
    }
    
    replacement.transforms.push(new Transform({
        type: type,
        editing: true,
	origin: origin,
        parameters: transformParams
    }));
    selectionManager.deselectAll();
    geom_doc.replace(original, replacement);
    return replacement;
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

    /*
     * Primitives
     */
    new Action('Cuboid', '/static/images/cuboid.png', 
               function() { 
		   SS.constructors.createCuboid(create_primitive('cuboid',  ['u', 'v', 'w']));
	       }).render($('#3Dprimitives'));
    new Action('Sphere', '/static/images/sphere.png', 
               function() { 
                   SS.constructors.createSphere(create_primitive('sphere',  ['r']));
	       }).render($('#3Dprimitives'));
    new Action('Cylinder', '/static/images/cylinder.png', 
               function() { 
		   SS.constructors.createCylinder(create_primitive('cylinder', ['r', 'h'])); 
	       }).render($('#3Dprimitives'));
    new Action('Cone', '/static/images/cone.png', 
               function() { 
		   create_primitive('cone', ['r1', 'h', 'r2']); 
		   SS.constructors.cone().create();
	       }).render($('#3Dprimitives'));
    new Action('Wedge', '/static/images/wedge.png', 
               function() { 
		   create_primitive('wedge', ['u1', 'v', 'u2', 'w']); 
		   SS.constructors.wedge().create();
	       }).render($('#3Dprimitives'));
    new Action('Torus', '/static/images/torus.png', 
               function() { 
		   SS.constructors.createTorus(create_primitive('torus', ['r1', 'r2']));
	       }).render($('#3Dprimitives'));

    new Action('Ellipse 1D', '/static/images/cuboid.png', 
               function() { 
                   SS.constructors.createEllipse1d(create_primitive('ellipse1d',  ['r1', 'r2']));
	       }).render($('#1Dprimitives'));
    
    /*
     * Booleans
     */
    new Action('Union', '/static/images/union.png', 
               function(selected) { boolean(selected, 'union'); }).render($('#boolean'));
    new Action('Subtract', '/static/images/diff.png', 
               function(selected) { boolean(selected, 'subtract'); }).render($('#boolean'));
    new Action('Intersect', '/static/images/intersect.png', 
               function(selected) { boolean(selected, 'intersect'); }).render($('#boolean'));
    
    /*
     * Transformations
     */
    new Action('Translate', '/static/images/translate.png', 
               function(selected) { 
		   var editingNode = create_transform(selected, 'translate', ['u', 'v', 'w', 'n']); 
		   SS.constructors.translate({geomNode: editingNode}).create();
	       }).render($('#transforms'));
    new Action('Scale', '/static/images/scale.png', 
               function(selected) { 
		   var editingNode = create_transform(selected, 'scale', ['factor']); 
		   SS.constructors.scale({geomNode: editingNode}).create();
	       }).render($('#transforms'));
    new Action('Rotate', '/static/images/rotate.png', 
               function(selected) { 
		   var editingNode = create_transform(selected, 'rotate', ['u', 'v', 'w', 'angle', 'n']);
		   SS.constructors.rotate({geomNode: editingNode}).create();
	       }).render($('#transforms'));
    new Action('Mirror', '/static/images/mirror.png', 
               function(selected) { 
		   var editingNode = create_transform(selected, 'mirror', ['u', 'v', 'w', 'n']); 
		   SS.constructors.mirror({geomNode: editingNode}).create();
	       }).render($('#transforms'));

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
