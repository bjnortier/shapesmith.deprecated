var SS = SS || {};

SS.constructors = {};

SS.constructors.sphere = function(spec) {
    var that = {};

    var focussed, previewGeometry;

    var onWorkplaneCursor = function(event) {
	
	if (focussed === 'origin') {
	    
	    $('#x').val(event.x);
	    $('#y').val(event.y);
	    updatePreview();

	} else if (focussed === 'r') {
	    var originX = $('#x').val(), originY = $('#y').val();
	    var x = event.x, y = event.y, r;
	    
	    var dx = Math.abs(x - originX), dy = Math.abs(y - originY);
	    var r  = Math.sqrt(dx*dx + dy*dy);
	    
	    $('#r').val(r.toFixed(2));
	    updatePreview();
	}
    }

    var onWorkplaneClicked = function(event) {
	if (focussed === 'origin') {
	    $('#modal-ok').focus();
	} else if (focussed === 'r') {
	    $('#modal-ok').focus();
	}
	
    }

    var setupValueHandlers = function() {
	$('#x').change(updatePreview);
	$('#y').change(updatePreview);
	$('#z').change(updatePreview);
	$('#r').change(updatePreview);
    }


    var setupFocusHandlers = function() {
	$('#r').focus(function() { focussed = 'r'; });
	$('#r').blur(function()  { focussed = undefined; });

	$('#x').focus(function() { focussed = 'origin'; });
	$('#y').focus(function() { focussed = 'origin'; });
	$('#z').focus(function() { focussed = 'origin'; });

	$('#x').blur(function() { focussed = undefined; });
	$('#y').blur(function() { focussed = undefined; });
	$('#z').blur(function() { focussed = undefined; });

	$('#modal-ok').focus(function() { focussed = 'ok'; });
	$('#model-ok').blur(function()  { focussed = undefined; });
	
    }

    var updatePreview = function() {
	if (previewGeometry) {
	    sceneView.scene.removeObject(previewGeometry);
	}

	previewGeometry = new THREE.Object3D();

	var x = parseFloat($('#x').val());
	var y = parseFloat($('#y').val());
	var r  =parseFloat($('#r').val());

	var geometry = new THREE.SphereGeometry(r, 50, 50);
	var material = new THREE.MeshBasicMaterial({color: 0x3F8FD2, opacity: 0.5});
	var sphere = new THREE.Mesh(geometry, material);
	previewGeometry.addChild(sphere);

	var circleGeom = new THREE.Geometry();
	for(var i = 0; i <= 50; ++i) {
	    var theta = Math.PI*2*i/50;
	    var dx = r*Math.cos(theta);
	    var dy = r*Math.sin(theta);
	    circleGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	}
	var circle = new THREE.Line(circleGeom, new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.5 }));
	previewGeometry.addChild(circle);
    
	previewGeometry.position.x = x;
	previewGeometry.position.y = y;
	sceneView.scene.addObject(previewGeometry);
    }
    
    that.create = function() {
	var lastMousePosition = sceneView.workplane.getLastMousePosition();

	setupValueHandlers();
	setupFocusHandlers();

	var originX = lastMousePosition.x;
	var originY = lastMousePosition.y;
	$('#x').val(originX);
	$('#y').val(originY);

	$('#r').focus();
	
	sceneView.workplane.on('workplaneCursorUpdated', onWorkplaneCursor);
	sceneView.workplane.on('workplaneClicked', onWorkplaneClicked);

	SS.constructors.active = this;
    }

    that.dispose = function() {
	sceneView.workplane.off('workplaneCursorUpdated', onWorkplaneCursor);
	sceneView.workplane.off('workplaneClicked', onWorkplaneClicked);
	
	if (previewGeometry) {
	    sceneView.scene.removeObject(previewGeometry);
	}
	SS.constructors.active = null;

    }
    
    return that;
}

SS.constructors.cylinder = function(spec) {
    var that = {};

    var focussed, previewGeometry;

    var onWorkplaneCursor = function(event) {
	
	if (focussed === 'origin') {
	    
	    $('#x').val(event.x);
	    $('#y').val(event.y);
	    updatePreview();

	} else if (focussed === 'r') {
	    var originX = $('#x').val(), originY = $('#y').val();
	    var x = event.x, y = event.y, r;
	    
	    var dx = Math.abs(x - originX), dy = Math.abs(y - originY);
	    var r  = Math.sqrt(dx*dx + dy*dy);
	    
	    $('#r').val(r.toFixed(2));
	    updatePreview();
	} else if (focussed === 'h') {
	    updatePreview();
	}

    }

    var onWorkplaneClicked = function(event) {
	if (focussed === 'origin') {
	    $('#modal-ok').focus();
	} else if (focussed === 'r') {
	    $('#h').focus();
	} else if (focussed === 'h') {
	    $('#modal-ok').focus();
	}

	
    }

    var setupValueHandlers = function() {
	$('#x').change(updatePreview);
	$('#y').change(updatePreview);
	$('#z').change(updatePreview);
	$('#r').change(updatePreview);
	$('#h').change(updatePreview);

    }


    var setupFocusHandlers = function() {
	$('#r').focus(function() { focussed = 'r'; });
	$('#r').blur(function()  { focussed = undefined; });

	$('#h').focus(function() { focussed = 'h'; });
	$('#h').blur(function()  { focussed = undefined; });

	$('#x').focus(function() { focussed = 'origin'; });
	$('#y').focus(function() { focussed = 'origin'; });
	$('#z').focus(function() { focussed = 'origin'; });

	$('#x').blur(function() { focussed = undefined; });
	$('#y').blur(function() { focussed = undefined; });
	$('#z').blur(function() { focussed = undefined; });

	$('#modal-ok').focus(function() { focussed = 'ok'; });
	$('#model-ok').blur(function()  { focussed = undefined; });
	
    }

    var updatePreview = function() {
	if (previewGeometry) {
	    sceneView.scene.removeObject(previewGeometry);
	}

	previewGeometry = new THREE.Object3D();

	var x = parseFloat($('#x').val());
	var y = parseFloat($('#y').val());
	var r = parseFloat($('#r').val());
	var h = parseFloat($('#h').val());

	if (h) {
	    var geometry = new THREE.CylinderGeometry(50, r, r, h);
	    var material = new THREE.MeshBasicMaterial({color: 0x3F8FD2, opacity: 0.5});
	    var cylinder = new THREE.Mesh(geometry, material);
	    cylinder.position.z = h/2;
	    previewGeometry.addChild(cylinder);
	}

	var circleGeom1 = new THREE.Geometry(), circleGeom2 = new THREE.Geometry();
	for(var i = 0; i <= 50; ++i) {
	    var theta = Math.PI*2*i/50;
	    var dx = r*Math.cos(theta);
	    var dy = r*Math.sin(theta);
	    circleGeom1.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	    circleGeom2.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	}
	var circle1 = new THREE.Line(circleGeom1, new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.5 }));
	previewGeometry.addChild(circle1);

	if (h) {
	    var circle2 = new THREE.Line(circleGeom2, new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.5 }));
	    circle2.position.z = h;
	    previewGeometry.addChild(circle2);
	}

	previewGeometry.position.x = x;
	previewGeometry.position.y = y;

	sceneView.scene.addObject(previewGeometry);
    }
    
    that.create = function() {
	var lastMousePosition = sceneView.workplane.getLastMousePosition();

	setupValueHandlers();
	setupFocusHandlers();

	var originX = lastMousePosition.x;
	var originY = lastMousePosition.y;
	$('#x').val(originX);
	$('#y').val(originY);

	$('#r').focus();
	
	sceneView.workplane.on('workplaneCursorUpdated', onWorkplaneCursor);
	sceneView.workplane.on('workplaneClicked', onWorkplaneClicked);

	SS.constructors.active = this;
    }

    that.dispose = function() {
	sceneView.workplane.off('workplaneCursorUpdated', onWorkplaneCursor);
	sceneView.workplane.off('workplaneClicked', onWorkplaneClicked);
	
	if (previewGeometry) {
	    sceneView.scene.removeObject(previewGeometry);
	}
	SS.constructors.active = null;

    }
    
    return that;
}
