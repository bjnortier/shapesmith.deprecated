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
    if (selected.length == 0)  {
        alert("please select at least one object");
        return;
    }
    var nodes = selected.map(function(path) {
	return geom_doc.findByPath(path);
    });
    selectionManager.deselectAll();

    var doFn = function() {
	for (var i in nodes) {
            geom_doc.remove(nodes[i]);
	}
	command_stack.inProgressSuccess();
    }

    var undoFn = function() {
	for (var i in nodes) {
            geom_doc.add(nodes[i]);
	}
	command_stack.inProgressSuccess();
    }

    var redoFn = doFn;
    var cmd = new Command(doFn, undoFn, redoFn);
    command_stack.execute(cmd);

}


function create_primitive(type, keys) {

    var geometryParams = {};
    for (var i in keys) {
        geometryParams[keys[i]] = null;
    }
    geom_doc.add(new GeomNode({
        type: type,
        editing: true,
	origin: {x: 0, y: 0, z: 0},
        parameters: geometryParams}));
}

function create_transform(selected, type, keys) {
    selectionManager.deselectAll();

    if (selected.length != 1)  {
        alert("no object selected!");
        return;
    }
    var transformParams = {};
    for (var i in keys) {
        transformParams[keys[i]] = null;
    }
    
    var path = selected[0];
    
    var original = geom_doc.findByPath(path);
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
    geom_doc.replace(original, replacement);
       
}


$(document).ready(function() {

    /*
     * Document
     */
    /*new Action("Save", "images/save.png", 
               function() { 
		   save(); 
	       }).render($("#document"));
    new Action("Undo", "images/undo.png", 
               function() { 
		   command_stack.undo(); 
	       }).render($("#document"));
    new Action("Redo", "images/redo.png", 
               function() { 
		   command_stack.redo(); 
	       }).render($("#document"));*/
    

    // Edit
    new Action("Delete", "images/trash.png", 
               function(selected) { 
		   delete_geom(selected); 
	       }).render($("#edit"));
    new Action("Copy", "/static/images/copy.png", 
               function(selected) { 
		   copy(selected)
	       }).render($("#edit"));
    new Action("Export to STL", "images/stl.png", 
               function(selected) { 
		   var pattern = /^\/geom\/(.*)$/;
		   var id = selectionManager.getSelected()[0].match(pattern)[1];
		   window.location = '/stl/' + id; 
	       }).render($("#edit"));

    /*
     * Primitives
     */
    new Action("Cuboid", "/static/images/cuboid.png", 
               function() { 
		   create_primitive("cuboid",  ["u", "v", "w"]); 
		   SS.constructors.cuboid().create();
	       }).render($("#primitives"));
    new Action("Sphere", "/static/images/sphere.png", 
               function() { 
		   create_primitive("sphere", ["r"]); 
		   SS.constructors.sphere().create();
	       }).render($("#primitives"));
    new Action("Cylinder", "/static/images/cylinder.png", 
               function() { 
		   create_primitive("cylinder", ["r", "h"]); 
		   SS.constructors.cylinder().create();
	       }).render($("#primitives"));
    new Action("Cone", "/static/images/cone.png", 
               function() { 
		   create_primitive("cone", ["r1", "h", "r2"]); 
		   SS.constructors.cone().create();
	       }).render($("#primitives"));
    new Action("Wedge", "/static/images/wedge.png", 
               function() { 
		   create_primitive("wedge", ["u1", "v", "u2", "w"]); 
		   SS.constructors.wedge().create();
	       }).render($("#primitives"));
    new Action("Torus", "/static/images/torus.png", 
               function() { 
		   create_primitive("torus", ["r1", "r2"]); 
		   SS.constructors.torus().create();
	       }).render($("#primitives"));
    
    /*
     * Booleans
     */
    new Action("Union", "/static/images/union.png", 
               function(selected) { boolean(selected, "union"); }).render($("#boolean"));
    new Action("Subtract", "/static/images/diff.png", 
               function(selected) { boolean(selected, "subtract"); }).render($("#boolean"));
    new Action("Intersect", "/static/images/intersect.png", 
               function(selected) { boolean(selected, "intersect"); }).render($("#boolean"));
    
    /*
     * Transformations
     */
    new Action("Translate", "/static/images/translate.png", 
               function(selected) { 
		   create_transform(selected, "translate", ["u", "v", "w", "n"]); 
		   SS.constructors.translate({selected: selected}).create();
	       }).render($("#transforms"));
    new Action("Scale", "/static/images/scale.png", 
               function(selected) { 
		   create_transform(selected, "scale", ["factor"]); 
		   SS.constructors.scale({selected: selected}).create();
	       }).render($("#transforms"));
    new Action("Rotate", "/static/images/rotate.png", 
               function(selected) { 
		   create_transform(selected, "rotate", ["u", "v", "w", "angle", "n"]);
		   SS.constructors.rotate({selected: selected}).create();
	       }).render($("#transforms"));
    new Action("Mirror", "/static/images/mirror.png", 
               function(selected) { 
		   create_transform(selected, "mirror", ["u", "v", "w", "n"]); 
		   SS.constructors.mirror({selected: selected}).create();
	       }).render($("#transforms"));



});

