function Action(label, iconPath, fn) {
    this.label = label;
    this.iconPath = iconPath;
    this.fn = fn;

    this.render = function(toolbar) {

        var imgId = "action_" + label.toLowerCase().replace(' ', '_');
        toolbar.append('<img id="' + imgId + '" src="' + this.iconPath + '" title="' + label + '"/>');
        
        // Because 'this' is the HTML element inside the function below,
        // we have to use a reference
        var fn = this.fn;
        jQuery("#" + imgId).click(function() {
            fn();
        });
    }
}

function delete_geom() {
    var selected = selectionManager.selected();
    selectionManager.deselectAll();
    for (var i in selected) {
        geom_doc.removeByPath(selected[i]);
    }
}


function create_primitive(type, keys) {
    var geometryParams = {};
    for (var i in keys) {
        geometryParams[keys[i]] = null;
    }
    geom_doc.add(new GeomNode({
        type: type,
        editing: true,
        parameters: geometryParams}));
}

function create_transform(type, keys) {
    if (selectionManager.size() != 1)  {
        alert("no object selected!");
        return;
    }
    var transformParams = {};
    for (var i in keys) {
        transformParams[keys[i]] = null;
    }
    
    var path = selectionManager.selected()[0];
    
    var original = geom_doc.findByPath(path);
    var replacement = original.editableCopy();
    replacement.transforms.push(new Transform({
        type: type,
        editing: true,
        parameters: transformParams
    }));
    geom_doc.replace(original, replacement);
       
}


$(document).ready(function() {

    /*
     * Basic
     */
    new Action("Save", "images/save.png", 
               function(parameters) { save(); }).render($("#edit"));
    new Action("Delete", "images/trash.png", 
               function(parameters) { delete_geom(); }).render($("#edit"));
    new Action("Undo", "images/undo.png", 
               function(parameters) { 
		   command_stack.undo(); 
	       }).render($("#edit"));
    new Action("Redo", "images/redo.png", 
               function(parameters) { 
		   command_stack.redo(); 
	       }).render($("#edit"));
    
    $('#edit').append('<a id="action_stl" href=""><img src="images/stl.png" alt="stl" title="Export to STL"></img></a>');


    $('#action_stl').click(function() {
        alert("select one object"); 
        return false;
    });
        
    
    /*
     * Primitives
     */
    new Action("Cuboid", "/images/cuboid.png", 
               function() { create_primitive("cuboid",  ["width", "depth", "height"]); }).render($("#primitives"));
    new Action("Sphere", "/images/sphere.png", 
               function(parameters) { create_primitive("sphere", ["radius"]); }).render($("#primitives"));
    new Action("Cylinder", "/images/cylinder.png", 
               function(parameters) { create_primitive("cylinder", ["radius", "height"]); }).render($("#primitives"));
    new Action("Cone", "/images/cone.png", 
               function(parameters) { create_primitive("cone", ["bottom_radius", "top_radius", "height"]); }).render($("#primitives"));
     new Action("Wedge", "/images/wedge.png", 
                function(parameters) { create_primitive("wedge", ["x1", "x2", "y", "z"]); }).render($("#primitives"));
    new Action("Torus", "/images/torus.png", 
               function(parameters) { create_primitive("torus", ["r1", "r2"]); }).render($("#primitives"));

    /*
     * Booleans
     */
    new Action("Union", "/images/union.png", 
               function(parameters) { boolean("union"); }).render($("#boolean"));
    new Action("Subtract", "/images/diff.png", 
               function(parameters) { boolean("subtract"); }).render($("#boolean"));
    new Action("Intersect", "/images/intersect.png", 
               function(parameters) { boolean("intersect"); }).render($("#boolean"));
    
    /*
     * Transformations
     */
    new Action("Translate", "/images/translate.png", 
               function(parameters) { create_transform("translate", ["dx", "dy", "dz"]); }).render($("#transforms"));
    new Action("Scale", "/images/scale.png", 
               function(parameters) { create_transform("scale", ["x", "y", "z", "factor"]); }).render($("#transforms"));
    new Action("Rotate", "/images/rotate.png", 
               function(parameters) { create_transform("rotate", ["px", "py", "pz", "vx", "vy", "vz", "angle"]); }).render($("#transforms"));
    new Action("Mirror", "/images/mirror.png", 
               function(parameters) { create_transform("mirror", ["px", "py", "pz", "vx", "vy", "vz"]); }).render($("#transforms"));

    /*
     * Copy & Transform
     */
    new Action("Copy", "/images/copy.png", 
               function(parameters) { 
		   copy()
	       }).render($("#copy_transforms"));
    new Action("Copy Translate", "/images/copy_translate.png", 
               function(parameters) { create_transform("copy_translate", ["dx", "dy", "dz", "n"]); }).render($("#copy_transforms"));
    new Action("Copy Rotate", "/images/copy_rotate.png", 
               function(parameters) { create_transform("copy_rotate", ["px", "py", "pz", "vx", "vy", "vz", "angle", "n"]); }).render($("#copy_transforms"));
    new Action("Copy Mirror", "/images/copy_mirror.png", 
               function(parameters) { create_transform("copy_mirror", ["px", "py", "pz", "vx", "vy", "vz"]); }).render($("#copy_transforms"));




});

