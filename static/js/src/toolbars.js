function Action(label, iconPath, fn) {
    this.label = label;
    this.iconPath = iconPath;
    this.fn = fn;

    this.render = function(toolbar) {

        var image = $('<img class="action" src="' + this.iconPath + '" title="' + label + '"/>');
        toolbar.append(image);

        image.click(function(event) {
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
    delete_geom_nodes(nodes);
}

function delete_geom_nodes(nodes) {
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

SS.creators = {};
SS.creators.sphere = SS.SphereCreator;
SS.creators.cuboid = SS.CuboidCreator;
SS.creators.cylinder = SS.CylinderCreator;
SS.creators.cone = SS.ConeCreator;
SS.creators.wedge = SS.WedgeCreator;
SS.creators.torus = SS.TorusCreator;
SS.creators.ellipse2d = SS.Ellipse2DCreator;
SS.creators.rectangle2d = SS.Rectangle2DCreator;
SS.creators.triangle2d = SS.Triangle2DCreator;
SS.creators.text2d = SS.Text2DCreator;
SS.creators.prism = SS.PrismCreator;
SS.creators.fillet = SS.FilletCreator;
SS.creators.revolve = SS.RevolveCreator;
SS.creators.bezier = SS.BezierCreator;
SS.creators.ellipse1d = SS.Ellipse1DCreator;
SS.creators.polyline = SS.PolylineCreator;

SS.creators.translate = SS.TranslateTransformer;
SS.creators.scale = SS.ScaleTransformer;
SS.creators.rotate = SS.RotateTransformer;
SS.creators.mirror = SS.AxisMirrorTransformCreator;
SS.creators.axis_mirror = SS.AxisMirrorTransformCreator;
SS.creators.plane_mirror = SS.PlaneMirrorTransformCreator;


function create_primitive(type) {

    var geometryParams = {}, geomNode;
    var jsonSchema = SS.schemas[type];

    var createParams = function(key, context, schema) {
        if (schema.type !== 'array') {
            context[key] = null;
        } else {
            context[key] = [];
            for (var i = 0; i < schema.minItems; ++i) {
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

    geomNode = new GeomNode({
        type: type,
        editing: true,
        origin: {
            x: SS.sceneView.lastClickedPositionOnWorkplane.x || 0,
            y: SS.sceneView.lastClickedPositionOnWorkplane.y || 0,
            z: 0
        },
        workplane: SS.workplaneModel.node.editableCopy(),
        parameters: geometryParams
    });
    geom_doc.add(geomNode);
    
    new SS.creators[type]({editingNode: geomNode}); 
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
    var parentNode = new GeomNode({
        type: type,
        editing: true,
        origin: jsonSchema.properties.origin ? {x: 0, y: 0, z: 0} : null,
        parameters: geometryParams,
        workplane: SS.workplaneModel.node.editableCopy(),
    }, [original]);
                                
    geom_doc.replace(original, parentNode);

    return {childNode: original, editingNode: parentNode};
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
        origin: {x: 0, y: 0, z:0},
        parameters: transformParams
    });
    replacement.transforms.push(transform);
    original.originalSceneObjects = original.sceneObjects;
    geom_doc.replace(original, replacement);
    return {originalNode: original, editingNode: replacement, transform: transform};
}



$(document).ready(function() {

    // Edit
    var deleteAction = new Action('Delete', '/static/images/trash.png', 
                                  function(selected) { 
		                      delete_geom(selected); 
	                          });
    deleteAction.render($('#edit'));
    deleteAction.render($('#boolean'));
    new Action('Copy', '/static/images/copy.png', 
               function(selected) { 
		   copy(selected)
	       }).render($('#edit'));

    //
    // Primitives
    //
    new Action('Cuboid', '/static/images/cuboid.png', 
               function() { 
		   create_primitive('cuboid');
	       }).render($('#3Dprimitives'));
    new Action('Sphere', '/static/images/sphere.png', 
               function() { 
                   create_primitive('sphere');
	       }).render($('#3Dprimitives'));
    new Action('Cylinder', '/static/images/cylinder.png', 
               function() { 
		   create_primitive('cylinder'); 
	       }).render($('#3Dprimitives'));
    new Action('Cone', '/static/images/cone.png', 
               function() { 
		   create_primitive('cone');
	       }).render($('#3Dprimitives'));
    new Action('Wedge', '/static/images/wedge.png', 
               function() { 
		   create_primitive('wedge');
	       }).render($('#3Dprimitives'));
    new Action('Torus', '/static/images/torus.png', 
               function() { 
		   create_primitive('torus');
	       }).render($('#3Dprimitives'));

    new Action('Ellipse 2D', '/static/images/ellipse2d.png', 
               function() { 
                   create_primitive('ellipse2d');
	       }).render($('#2Dprimitives'));
    new Action('Rectangle 2D', '/static/images/rectangle2d.png', 
               function() { 
                   create_primitive('rectangle2d');
	       }).render($('#2Dprimitives'));
    new Action('Triangle 2D', '/static/images/triangle2d.png', 
               function() { 
                   create_primitive('triangle2d');
	       }).render($('#2Dprimitives'));
    new Action('Text 2D', '/static/images/text2d.png', 
               function() { 
                   create_primitive('text2d');
	       }).render($('#2Dprimitives'));

    new Action('Ellipse 1D', '/static/images/ellipse1d.png',
               function() {
                   create_primitive('ellipse1d');
               }).render($('#1Dprimitives'));
    new Action('Polyline', '/static/images/polyline.png',
               function() {
                   create_primitive('polyline');
               }).render($('#1Dprimitives'));
    new Action('Bezier', '/static/images/bezier.png',
               function() {
                   create_primitive('bezier');
               }).render($('#1Dprimitives'));

    
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
    new Action('Loft', '/static/images/loft.png', 
               function(selected) { 
                   boolean(selected, 'loft'); 
               }).render($('#boolean'));
    //
    // Modifiers
    //
    new Action('Prism', '/static/images/prism.png', 
               function(selected) { 
                   new SS.PrismCreator(create_modifier(selected, 'prism'));
               }).render($('#modifiers'));
    new Action('Revolve', '/static/images/revolve.png', 
               function(selected) { 
                   new SS.RevolveCreator(create_modifier(selected, 'revolve'));
               }).render($('#modifiers'));
    new Action('Fillet', '/static/images/fillet.png', 
               function(selected) { 
                   new SS.FilletCreator(create_modifier(selected, 'fillet'));
               }).render($('#modifiers'));
    var createFaceAction = new Action('Create Face', '/static/images/make_face.png', 
                                      function(selected) { 
                                          boolean(selected, 'make_face'); 
                                      });
    var createSolidAction = new Action('Create Solid', '/static/images/make_solid.png', 
                                       function(selected) { 
                                          boolean(selected, 'make_solid'); 
                                       });
    createFaceAction.render($('#modifiers'));
    createFaceAction.render($('#boolean'));
    createSolidAction.render($('#modifiers'));
    createSolidAction.render($('#boolean'));
    
    //
    // Transformations
    //
    new Action('AxisMirror', '/static/images/axis_mirror.png', 
               function(selected) { 
                   var originalReplacementAndTransform = create_transform(selected, 'axis_mirror');
		   new SS.AxisMirrorTransformCreator(originalReplacementAndTransform);
	       }).render($('#transforms'));
    new Action('PlaneMirror', '/static/images/plane_mirror.png', 
               function(selected) { 
                   var originalReplacementAndTransform = create_transform(selected, 'plane_mirror');
		   new SS.PlaneMirrorTransformCreator(originalReplacementAndTransform);
	       }).render($('#transforms'));
    
});


$('#action-save').click(function() {
    SS.save();
});

$('#action-import-json').click(function() {
    $('#json-file-select-input').click();
});

$('#action-import-stl').click(function() {
    $('#stl-file-select-input').click();
});

(function() {

    var uploadFile = function(input, successFn) {
        if (!window.FileReader) {
          alert('No FileReader support yet in this browser. Please try Google Chrome or Firefox');
          return;
        }
        var reader = new FileReader();

        // Closure to capture the file information.
        reader.onload = (function(theFile) {
            return function(e) {
                if (e.target.readyState === 2) {
                    input.value = "";
                    successFn(e.target.result);
                }
            };
        })(input.files[0]);

        // Read in the file as a data URL.
        reader.readAsBinaryString(input.files[0]);
    }

    $('#json-file-select-input').change(function() {
        uploadFile(this, importJSON);
    });

    $('#stl-file-select-input').change(function() {
        uploadFile(this, function(contents) {
            var geomNode = new GeomNode({
                type: 'import_stl',
                contents: btoa(contents)
            });
            // No prototype for STL import
            var cmd = create_geom_command(undefined, geomNode);
            command_stack.execute(cmd);
        })
    });
})();
    

$('#action-export-stl').click(function() {
    window.location = '/' + SS.session.username + '/' + SS.session.design + '/stl/' + SS.session.commit + '/';
});

$('#action-export-json').click(function() {
    window.location = '/' + SS.session.username + '/' + SS.session.design + '/json/' + SS.session.commit + '/';
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
