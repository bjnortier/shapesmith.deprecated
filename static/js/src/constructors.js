var SS = SS || {};

SS.constructors = {};

SS.constructors.lineColor = 0x66A1D2;
SS.constructors.faceColor = 0x3F8FD2;
SS.constructors.faceMaterial = new THREE.MeshBasicMaterial( { color: SS.constructors.faceColor, opacity: 0.5 } );
SS.constructors.solidFaceMaterial = new THREE.MeshBasicMaterial( { color: SS.constructors.faceColor } );
SS.constructors.lineMaterial = new THREE.LineBasicMaterial({ color: SS.constructors.lineColor, wireframe : true });
SS.constructors.wireframeMaterial = new THREE.MeshBasicMaterial( { color: SS.constructors.lineColor, wireframe: true } )

SS.constructors.origin = function(my) {
    var that = {};

    that.onWorkplaneCursorUpdated = function (event) {
	if (my.focussed === 'origin') {
	    $('#x').val(event.x);
	    $('#y').val(event.y);
	    $('#z').val(0);
	    return true;
	}
	return false;
    }

    that.onWorkplaneClicked = function(focusSequence) {
	var nextFound = false;
	var next = my.focussed;
	do {
	    next = focusSequence[next];
	    if (next) {
		if ($('#' + next).val() === '') {
		    $('#' + next).focus();
		    nextFound = true;
		}
	    } else {
		$('#modal-ok').focus();
	    }
	    
	} while(!nextFound && next);
    }

    that.updatePreview = function() {
	if (my.previewGeometry) {
	    sceneView.scene.removeObject(my.previewGeometry);
	}
	my.previewGeometry = new THREE.Object3D();

	var pointerMaterial = [ SS.constructors.faceMaterial, SS.constructors.wireframeMaterial ];
	var pointerGeometry = new THREE.CubeGeometry(0.5, 0.5, 0.5);
	var pointer = new THREE.Mesh(pointerGeometry, pointerMaterial);
	

	my.previewGeometry.addChild(pointer);
    }

    that.setupValueHandlers = function() {
	$('#x').keyup(my.updatePreview);
	$('#y').keyup(my.updatePreview);
	$('#z').keyup(my.updatePreview);
    }

    that.setupFocusHandlers = function() {
	$('#x').focus(function() { my.focussed = 'origin'; });
	$('#y').focus(function() { my.focussed = 'origin'; });
	$('#z').focus(function() { my.focussed = 'origin'; });

	$('#x').blur(function() { my.focussed = undefined; });
	$('#y').blur(function() { my.focussed = undefined; });
	$('#z').blur(function() { my.focussed = undefined; });

	$('#modal-ok').focus(function() { my.focussed = 'ok'; });
	$('#model-ok').blur(function()  { my.focussed = undefined; });
    }

    that.edit = function() {
	that.setupValueHandlers();
	that.setupFocusHandlers();
	sceneView.workplane.on('workplaneXYCursorUpdated', function(event) {
	    if (event.originalEvent.shiftKey) {
		that.onWorkplaneCursorUpdated(event);
	    }
	});
	sceneView.workplane.on('workplaneClicked', that.onWorkplaneClicked);

	SS.constructors.active = that;
    }

    that.create = function() {
	that.edit();
    }
    
    that.dispose = function() {
	sceneView.workplane.off('workplaneXYCursorUpdated', that.onWorkplaneCursorUpdated);
	sceneView.workplane.off('workplaneClicked', that.onWorkplaneClicked);
	
	if (my.previewGeometry) {
	    sceneView.scene.removeObject(my.previewGeometry);
	}
	SS.constructors.active = null;
    }

    return that;
}

SS.constructors.primitive = function(my) {
    var that = SS.constructors.origin(my);

    var superCreate = that.create;
    that.create = function() {
	superCreate();
	var lastMousePosition = sceneView.workplane.getLastMousePosition();
	var originX = lastMousePosition.x;
	var originY = lastMousePosition.y;
	$('#x').val(originX);
	$('#y').val(originY);
    }

    return that;
}

SS.constructors.cuboid = function(spec) {

    var my = {};
    var that = SS.constructors.primitive(my);

    var superUpdatePreview = that.updatePreview;
    var updatePreview = function() {

	superUpdatePreview();

	var x = parseFloat($('#x').val()) || 0.0;
	var y = parseFloat($('#y').val()) || 0.0;
	var z = parseFloat($('#z').val()) || 0.0;

	var u = parseFloat($('#u').val());
	var v = parseFloat($('#v').val());
	var w = parseFloat($('#w').val());

	if (u && v && w) {
	    var geometry = new THREE.CubeGeometry(u,v,w);
	    var materials = [ SS.constructors.faceMaterial, SS.constructors.wireframeMaterial ];
	    var cube = new THREE.Mesh(geometry, materials);
	    cube.position.x = u/2;
	    cube.position.y = v/2;
	    cube.position.z = w/2;
	    my.previewGeometry.addChild(cube);

	} else if (u && v) {
	    var uvLineGeom = new THREE.Geometry();
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    var uvLine = new THREE.Line(uvLineGeom, SS.constructors.lineMaterial);
	    my.previewGeometry.addChild(uvLine);

	} else if (u) {
	    var uLineGeom = new THREE.Geometry();
	    uLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    uLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u, 0, 0)));
	    var uLine = new THREE.Line(uLineGeom, SS.constructors.lineMaterial);
	    my.previewGeometry.addChild(uLine);
	}

	my.previewGeometry.position.x = x;
	my.previewGeometry.position.y = y;
	my.previewGeometry.position.z = z;

	sceneView.scene.addObject(my.previewGeometry);
	
    }
    my.updatePreview = updatePreview;

    var superOnWorkplaneCursorUpdated = that.onWorkplaneCursorUpdated;
    that.onWorkplaneCursorUpdated = function(event) {
	
	if (superOnWorkplaneCursorUpdated(event)) {
	    updatePreview();
	} else if (my.focussed === 'u') {
	    var originX = parseFloat($('#x').val()), originY = parseFloat($('#y').val());
	    var x = event.x, y = event.y;
	    
	    var u = x - originX, v = y - originY;
	    
	    $('#u').val(u.toFixed(2));
	    updatePreview();

	} else if (my.focussed === 'v') {
	    var originX = parseFloat($('#x').val()), originY = parseFloat($('#y').val());
	    var x = event.x, y = event.y;
	    
	    var v = y - originY;
	    
	    $('#v').val(v.toFixed(2));
	    updatePreview();

	} else if (my.focussed === 'w') {

	    var originX = parseFloat($('#x').val()), originY = parseFloat($('#y').val());

	    var origin = new THREE.Vector3(originX, originY, 0);
	    var direction = new THREE.Vector3(0, 0, 1);
	    var ray = new THREE.Ray(origin, direction);
	    var positionOnVertical = sceneView.determinePositionOnRay(event.originalEvent, ray);

	    $('#w').val(positionOnVertical.z.toFixed(0));

	    updatePreview();
	}

    }

    var superOnWorkplaneClicked = that.onWorkplaneClicked;
    that.onWorkplaneClicked = function(event) {
	superOnWorkplaneClicked({origin: 'u', u:'v', v:'w'});
    }
    
    var superSetupValueHandlers = that.setupValueHandlers;
    that.setupValueHandlers = function() {
	superSetupValueHandlers(updatePreview);
	$('#u').keyup(updatePreview);
	$('#v').keyup(updatePreview);
	$('#w').keyup(updatePreview);
    }

    var superSetupFocusHandlers = that.setupFocusHandlers;
    that.setupFocusHandlers = function() {
	superSetupFocusHandlers();

	$('#u').focus(function() { my.focussed = 'u'; });
	$('#u').blur(function()  { my.focussed = undefined; });

	$('#v').focus(function() { my.focussed = 'v'; });
	$('#v').blur(function()  { my.focussed = undefined; });

	$('#w').focus(function() { my.focussed = 'w'; });
	$('#w').blur(function()  { my.focussed = undefined; });
    }

    var superCreate = that.create;
    that.create = function() {
	superCreate();
	$('#u').focus();
	updatePreview();
    }

    return that;
}

SS.constructors.sphere = function() {

    var my = {};
    var that = SS.constructors.primitive(my);

    var superUpdatePreview = that.updatePreview;
    var updatePreview = function() {

	superUpdatePreview();
	
	var x = parseFloat($('#x').val()) || 0.0;
	var y = parseFloat($('#y').val()) || 0.0;
	var z = parseFloat($('#z').val()) || 0.0;
	var r = parseFloat($('#r').val()) || 0.001;

	var geometry = new THREE.SphereGeometry(r, 50, 10);
	var sphere = new THREE.Mesh(geometry, SS.constructors.faceMaterial);
	my.previewGeometry.addChild(sphere);

	var circleGeom = new THREE.Geometry();
	for(var i = 0; i <= 50; ++i) {
	    var theta = Math.PI*2*i/50;
	    var dx = r*Math.cos(theta);
	    var dy = r*Math.sin(theta);
	    circleGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	}
	var circle = new THREE.Line(circleGeom, SS.constructors.lineMaterial);
	my.previewGeometry.addChild(circle);
    
	my.previewGeometry.position.x = x;
	my.previewGeometry.position.y = y;
	my.previewGeometry.position.z = z;
	sceneView.scene.addObject(my.previewGeometry);
    }
    my.updatePreview = updatePreview;

    var superOnWorkplaneCursorUpdated = that.onWorkplaneCursorUpdated;
    that.onWorkplaneCursorUpdated = function(event) {

	if (superOnWorkplaneCursorUpdated(event)) {
	    updatePreview();
	    return;
	}
	
	var originX = parseFloat($('#x').val()), originY = parseFloat($('#y').val());
	if (my.focussed === 'r') {
	    
	    var x = event.x, y = event.y;
	    
	    var dx = Math.abs(x - originX), dy = Math.abs(y - originY);
	    var r  = Math.sqrt(dx*dx + dy*dy);
	    
	    $('#r').val(r.toFixed(2));
	    updatePreview();
	}
    }

    var superOnWorkplaneClicked = that.onWorkplaneClicked;
    that.onWorkplaneClicked = function(event) {
	superOnWorkplaneClicked({origin: 'r'});
    }

    var superSetupValueHandlers = that.setupValueHandlers;
    that.setupValueHandlers = function() {
	superSetupValueHandlers();
	$('#r').keyup(updatePreview);
    }

    var superSetupFocusHandlers = that.setupFocusHandlers;
    that.setupFocusHandlers = function() {
	superSetupFocusHandlers();
	$('#r').focus(function() { my.focussed = 'r'; });
	$('#r').blur(function()  { my.focussed = undefined; });
    }

    var superCreate = that.create;
    that.create = function() {
	superCreate();
	$('#r').focus();
	updatePreview();
    }
   
    return that;
}

SS.constructors.cylinder = function(spec) {

    var my = {};
    var that = SS.constructors.primitive(my);

    var superUpdatePreview = that.updatePreview;
    var updatePreview = function() {

	superUpdatePreview();

	var x = parseFloat($('#x').val()) || 0.0;
	var y = parseFloat($('#y').val()) || 0.0;
	var z = parseFloat($('#z').val()) || 0.0;

	var r = parseFloat($('#r').val());
	var h = parseFloat($('#h').val());

	if (r) {
	    var circleGeom1 = new THREE.Geometry(), circleGeom2 = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
		var theta = Math.PI*2*i/50;
		var dx = r*Math.cos(theta);
		var dy = r*Math.sin(theta);
		circleGeom1.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
		circleGeom2.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	    }
	    var circle1 = new THREE.Line(circleGeom1, SS.constructors.lineMaterial);
	    my.previewGeometry.addChild(circle1);
	}

	if (r && h) {
	    var circle2 = new THREE.Line(circleGeom2, SS.constructors.lineMaterial);
	    circle2.position.z = h;
	    my.previewGeometry.addChild(circle2);

	    var geometry = new THREE.CylinderGeometry(50, r, r, h);
	    var cylinder = new THREE.Mesh(geometry, SS.constructors.faceMaterial);
	    cylinder.position.z = h/2;
	    my.previewGeometry.addChild(cylinder);
	}

	my.previewGeometry.position.x = x;
	my.previewGeometry.position.y = y;
	my.previewGeometry.position.z = z;

	sceneView.scene.addObject(my.previewGeometry);
	
    }
    my.updatePreview = updatePreview;

    var superOnWorkplaneCursorUpdated = that.onWorkplaneCursorUpdated;
    that.onWorkplaneCursorUpdated = function(event) {
	
	if (superOnWorkplaneCursorUpdated(event)) {
	    updatePreview();
	    return;
	}
	
	var originX = parseFloat($('#x').val()), originY = parseFloat($('#y').val());

	if (my.focussed === 'r') {
	    var x = event.x, y = event.y;
	    
	    var dx = Math.abs(x - originX), dy = Math.abs(y - originY);
	    var r  = Math.sqrt(dx*dx + dy*dy);
	    
	    $('#r').val(r.toFixed(2));
	    updatePreview();
	} else if (my.focussed === 'h') {

	    var origin = new THREE.Vector3(originX, originY, 0);
	    var direction = new THREE.Vector3(0, 0, 1);
	    var ray = new THREE.Ray(origin, direction);
	    var positionOnVertical = sceneView.determinePositionOnRay(event.originalEvent, ray);

	    $('#h').val(positionOnVertical.z.toFixed(0));

	    updatePreview();
	}

    }

    var superOnWorkplaneClicked = that.onWorkplaneClicked;
    that.onWorkplaneClicked = function(event) {
	superOnWorkplaneClicked({origin: 'r', r:'h'});
    }

    var superSetupValueHandlers = that.setupValueHandlers;
    that.setupValueHandlers = function() {
	superSetupValueHandlers(updatePreview);
	$('#r').keyup(updatePreview);
	$('#h').keyup(updatePreview);
    }

    var superSetupFocusHandlers = that.setupFocusHandlers;
    that.setupFocusHandlers = function() {
	superSetupFocusHandlers();

	$('#r').focus(function() { my.focussed = 'r'; });
	$('#r').blur(function()  { my.focussed = undefined; });

	$('#h').focus(function() { my.focussed = 'h'; });
	$('#h').blur(function()  { my.focussed = undefined; });
    }

    var superCreate = that.create;
    that.create = function() {
	superCreate();
	$('#r').focus();
	updatePreview();
    }

    return that;
}

SS.constructors.cylinder = function(spec) {

    var my = {};
    var that = SS.constructors.primitive(my);

    var superUpdatePreview = that.updatePreview;
    var updatePreview = function() {

	superUpdatePreview();

	var x = parseFloat($('#x').val()) || 0.0;
	var y = parseFloat($('#y').val()) || 0.0;
	var z = parseFloat($('#z').val()) || 0.0;

	var r = parseFloat($('#r').val());
	var h = parseFloat($('#h').val());

	if (r) {
	    var circleGeom1 = new THREE.Geometry(), circleGeom2 = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
		var theta = Math.PI*2*i/50;
		var dx = r*Math.cos(theta);
		var dy = r*Math.sin(theta);
		circleGeom1.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
		circleGeom2.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	    }
	    var circle1 = new THREE.Line(circleGeom1, SS.constructors.lineMaterial);
	    my.previewGeometry.addChild(circle1);
	}

	if (r && h) {
	    var circle2 = new THREE.Line(circleGeom2, SS.constructors.lineMaterial);
	    circle2.position.z = h;
	    my.previewGeometry.addChild(circle2);

	    var geometry = new THREE.CylinderGeometry(50, r, r, h);
	    var cylinder = new THREE.Mesh(geometry, SS.constructors.faceMaterial);
	    cylinder.position.z = h/2;
	    my.previewGeometry.addChild(cylinder);
	}

	my.previewGeometry.position.x = x;
	my.previewGeometry.position.y = y;
	my.previewGeometry.position.z = z;

	sceneView.scene.addObject(my.previewGeometry);
	
    }
    my.updatePreview = updatePreview;

    var superOnWorkplaneCursorUpdated = that.onWorkplaneCursorUpdated;
    that.onWorkplaneCursorUpdated = function(event) {
	
	if (superOnWorkplaneCursorUpdated(event)) {
	    updatePreview();
	    return;
	}
	
	var originX = parseFloat($('#x').val()), originY = parseFloat($('#y').val());

	if (my.focussed === 'r') {
	    var x = event.x, y = event.y;
	    
	    var dx = Math.abs(x - originX), dy = Math.abs(y - originY);
	    var r  = Math.sqrt(dx*dx + dy*dy);
	    
	    $('#r').val(r.toFixed(2));
	    updatePreview();
	} else if (my.focussed === 'h') {

	    var origin = new THREE.Vector3(originX, originY, 0);
	    var direction = new THREE.Vector3(0, 0, 1);
	    var ray = new THREE.Ray(origin, direction);
	    var positionOnVertical = sceneView.determinePositionOnRay(event.originalEvent, ray);

	    $('#h').val(positionOnVertical.z.toFixed(0));

	    updatePreview();
	}

    }

    var superOnWorkplaneClicked = that.onWorkplaneClicked;
    that.onWorkplaneClicked = function(event) {
	superOnWorkplaneClicked({origin: 'r', r:'h'});
    }

    var superSetupValueHandlers = that.setupValueHandlers;
    that.setupValueHandlers = function() {
	superSetupValueHandlers(updatePreview);
	$('#r').keyup(updatePreview);
	$('#h').keyup(updatePreview);
    }

    var superSetupFocusHandlers = that.setupFocusHandlers;
    that.setupFocusHandlers = function() {
	superSetupFocusHandlers();

	$('#r').focus(function() { my.focussed = 'r'; });
	$('#r').blur(function()  { my.focussed = undefined; });

	$('#h').focus(function() { my.focussed = 'h'; });
	$('#h').blur(function()  { my.focussed = undefined; });
    }

    var superCreate = that.create;
    that.create = function() {
	superCreate();
	$('#r').focus();
	updatePreview();
    }

    return that;
}

SS.constructors.ellipse1d = function(spec) {

    var my = {};
    var that = SS.constructors.primitive(my);

    var superUpdatePreview = that.updatePreview;
    var updatePreview = function() {

	superUpdatePreview();

	var x = parseFloat($('#x').val()) || 0.0;
	var y = parseFloat($('#y').val()) || 0.0;
	var z = parseFloat($('#z').val()) || 0.0;

	var r1 = parseFloat($('#r1').val());
	var r2 = parseFloat($('#r2').val());

	if (r1 && r2) {

	    var ellipseGeom = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
		var theta = Math.PI*2*i/50;
		var dx = r1*Math.cos(theta);
		var dy = r2*Math.sin(theta);
		ellipseGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	    }
	    var ellipse = new THREE.Line(ellipseGeom, SS.constructors.lineMaterial);
	    my.previewGeometry.addChild(ellipse);
	}

	my.previewGeometry.position.x = x;
	my.previewGeometry.position.y = y;
	my.previewGeometry.position.z = z;

	sceneView.scene.addObject(my.previewGeometry);
	
    }
    my.updatePreview = updatePreview;

    var superOnWorkplaneCursorUpdated = that.onWorkplaneCursorUpdated;
    that.onWorkplaneCursorUpdated = function(event) {

	if (superOnWorkplaneCursorUpdated(event)) {
	    updatePreview();
	    return;
	}

        else if ((my.focussed === 'r1') || (my.foxussed === 'r1')) {
	    var originX = parseFloat($('#x').val()), originY = parseFloat($('#y').val());
	    var x = event.x, y = event.y;
            
	    var r1 = Math.abs(x - originX);
            var r2 = Math.abs(y - originY);
	    $('#r2').val(r2.toFixed(2));
	    $('#r1').val(r1.toFixed(2));
	    updatePreview();
        }

    }

    var superOnWorkplaneClicked = that.onWorkplaneClicked;
    that.onWorkplaneClicked = function(event) {
	superOnWorkplaneClicked({origin: 'r1', r1:'h', h:'r2'});
    }

    var superSetupValueHandlers = that.setupValueHandlers;
    that.setupValueHandlers = function() {
	superSetupValueHandlers();
	$('#r1').keyup(updatePreview);
	$('#h').keyup(updatePreview);
	$('#r2').keyup(updatePreview);
    }

    var superSetupFocusHandlers = that.setupFocusHandlers;
    that.setupFocusHandlers = function() {
	superSetupFocusHandlers();

	$('#r1').focus(function() { my.focussed = 'r1'; });
	$('#r1').blur(function()  { my.focussed = undefined; });

	$('#h').focus(function() { my.focussed = 'h'; });
	$('#h').blur(function()  { my.focussed = undefined; });

	$('#r2').focus(function() { my.focussed = 'r2'; });
	$('#r2').blur(function()  { my.focussed = undefined; });

    }

    var superCreate = that.create;
    that.create = function() {
	superCreate();
	$('#r1').focus();
	updatePreview();
    }

    return that;
}


SS.constructors.wedge = function(spec) {

    var my = {};
    var that = SS.constructors.primitive(my);

    var superUpdatePreview = that.updatePreview;
    var updatePreview = function() {

	superUpdatePreview();

	var x = parseFloat($('#x').val()) || 0.0;
	var y = parseFloat($('#y').val()) || 0.0;
	var z = parseFloat($('#z').val()) || 0.0;
	var u1 = parseFloat($('#u1').val());
	var u2 = parseFloat($('#u2').val());
	var v = parseFloat($('#v').val());
	var w = parseFloat($('#w').val());

	if (u1 && v && w) {
	    var geometry = new THREE.WedgeGeometry(u1,v,w,u2 - u1);
	    var materials = [ SS.constructors.faceMaterial, SS.constructors.wireframeMaterial ];
	    var wedge = new THREE.Mesh(geometry, materials);
	    wedge.position.x = u1/2;
	    wedge.position.y = v/2;
	    wedge.position.z = w/2;
	    my.previewGeometry.addChild(wedge);

	} else if (u1 && v && u2) {

	    var uvLineGeom = new THREE.Geometry();
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u1, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u2, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    var uvLine = new THREE.Line(uvLineGeom, SS.constructors.lineMaterial);
	    my.previewGeometry.addChild(uvLine);
	} else if (u1 && v) {
	    var uvLineGeom = new THREE.Geometry();
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u1, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u1, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    var uvLine = new THREE.Line(uvLineGeom, SS.constructors.lineMaterial);
	    my.previewGeometry.addChild(uvLine);

	} else if (u1) {
	    var uLineGeom = new THREE.Geometry();
	    uLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    uLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u1, 0, 0)));
	    var uLine = new THREE.Line(uLineGeom, SS.constructors.lineMaterial);
	    my.previewGeometry.addChild(uLine);
	}

	my.previewGeometry.position.x = x;
	my.previewGeometry.position.y = y;
	my.previewGeometry.position.z = z;

	sceneView.scene.addObject(my.previewGeometry);
	
    }
    my.updatePreview = updatePreview;

    var superOnWorkplaneCursorUpdated = that.onWorkplaneCursorUpdated;
    that.onWorkplaneCursorUpdated = function(event) {
	
	if (superOnWorkplaneCursorUpdated(event)) {
	    updatePreview();
	    return;
	}

	var originX = parseFloat($('#x').val()), originY = parseFloat($('#y').val());
	var x = event.x, y = event.y;
	
	if (my.focussed === 'u1') {
	    var u = x - originX, v = y - originY;
	    $('#u1').val(u.toFixed(2));
	    updatePreview();

	} else if (my.focussed === 'v') {
	    var v = y - originY;
	    $('#v').val(v.toFixed(2));
	    updatePreview();
	}

	else if (my.focussed === 'u2') {
	    var u = x - originX, v = y - originY;
	    $('#u2').val(u.toFixed(2));
	    updatePreview();

	} else if (my.focussed === 'w') {

	    var originX = parseFloat($('#x').val()), originY = parseFloat($('#y').val());

	    var origin = new THREE.Vector3(originX, originY, 0);
	    var direction = new THREE.Vector3(0, 0, 1);
	    var ray = new THREE.Ray(origin, direction);
	    var positionOnVertical = sceneView.determinePositionOnRay(event.originalEvent, ray);

	    $('#w').val(positionOnVertical.z.toFixed(0));

	    updatePreview();
	}

    }

    var superOnWorkplaneClicked = that.onWorkplaneClicked;
    that.onWorkplaneClicked = function(event) {
	superOnWorkplaneClicked({origin: 'u1', u1:'v', v:'u2', u2:'w'});
    }
    
    var superSetupValueHandlers = that.setupValueHandlers;
    that.setupValueHandlers = function() {
	superSetupValueHandlers(updatePreview);
	$('#u1').keyup(updatePreview);
	$('#v').keyup(updatePreview);
	$('#u2').keyup(updatePreview);
	$('#w').keyup(updatePreview);
    }

    var superSetupFocusHandlers = that.setupFocusHandlers;
    that.setupFocusHandlers = function() {
	superSetupFocusHandlers();

	$('#u1').focus(function() { my.focussed = 'u1'; });
	$('#u1').blur(function()  { my.focussed = undefined; });

	$('#v').focus(function() { my.focussed = 'v'; });
	$('#v').blur(function()  { my.focussed = undefined; });

	$('#u2').focus(function() { my.focussed = 'u2'; });
	$('#u2').blur(function()  { my.focussed = undefined; });

	$('#w').focus(function() { my.focussed = 'w'; });
	$('#w').blur(function()  { my.focussed = undefined; });
    }

    var superCreate = that.create;
    that.create = function() {
	superCreate();
	$('#u1').focus();
	updatePreview();
    }

    return that;
}

SS.constructors.torus = function(spec) {

    var my = {};
    var that = SS.constructors.primitive(my);

    var superUpdatePreview = that.updatePreview;
    var updatePreview = function() {

	superUpdatePreview();

	var x = parseFloat($('#x').val()) || 0.0;
	var y = parseFloat($('#y').val()) || 0.0;
	var z = parseFloat($('#z').val()) || 0.0;
	var r1 = parseFloat($('#r1').val());
	var r2 = parseFloat($('#r2').val());

	if (r1) {
	    var circleGeom1 = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
		var theta = Math.PI*2*i/50;
		var dx = r1*Math.cos(theta);
		var dy = r1*Math.sin(theta);
		circleGeom1.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	    }
	    var circle1 = new THREE.Line(circleGeom1, SS.constructors.lineMaterial);
	    my.previewGeometry.addChild(circle1);
	}

	if (r1 && r2) {

	    var circleGeom1 = new THREE.Geometry(), circleGeom2 = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
		var theta = Math.PI*2*i/50;
		var dx1 = (r1 + r2)*Math.cos(theta);
		var dy1 = (r1 + r2)*Math.sin(theta);
		var dx2 = (r1 - r2)*Math.cos(theta);
		var dy2 = (r1 - r2)*Math.sin(theta);
		circleGeom1.vertices.push(new THREE.Vertex(new THREE.Vector3(dx1, dy1, 0)));
		circleGeom2.vertices.push(new THREE.Vertex(new THREE.Vector3(dx2, dy2, 0)));
	    }
	    var circle1 = new THREE.Line(circleGeom1, SS.constructors.lineMaterial);
	    var circle2 = new THREE.Line(circleGeom2, SS.constructors.lineMaterial);
	    my.previewGeometry.addChild(circle1);
	    my.previewGeometry.addChild(circle2);

	    var geometry = new THREE.TorusGeometry(r1, r2, 10, 50);
	    var torus = new THREE.Mesh(geometry, SS.constructors.faceMaterial);
	    my.previewGeometry.addChild(torus);
	}

	my.previewGeometry.position.x = x;
	my.previewGeometry.position.y = y;
	my.previewGeometry.position.z = z;

	sceneView.scene.addObject(my.previewGeometry);
	
    }
    my.updatePreview = updatePreview;

    var superOnWorkplaneCursorUpdated = that.onWorkplaneCursorUpdated;
    that.onWorkplaneCursorUpdated = function(event) {
	
	if (superOnWorkplaneCursorUpdated(event)) {
	    updatePreview();
	    return;
	}
	
	var originX = parseFloat($('#x').val()), originY = parseFloat($('#y').val());
	var x = event.x, y = event.y;

	if (my.focussed === 'r1') {
	    
	    var dx = Math.abs(x - originX), dy = Math.abs(y - originY);
	    var r1  = Math.sqrt(dx*dx + dy*dy);
	    $('#r1').val(r1.toFixed(2));

	    updatePreview();

	} else if (my.focussed === 'r2') {

	    var dx = Math.abs(x - originX), dy = Math.abs(y - originY);
	    var r1 = parseFloat($('#r1').val());
	    var r2  = Math.abs(r1 - Math.sqrt(dx*dx + dy*dy));
	    $('#r2').val(r2.toFixed(2));

	    updatePreview();
	}

    }

    var superOnWorkplaneClicked = that.onWorkplaneClicked;
    that.onWorkplaneClicked = function(event) {
	superOnWorkplaneClicked({origin: 'r1', r1:'r2'});
    }

    var superSetupValueHandlers = that.setupValueHandlers;
    that.setupValueHandlers = function() {
	superSetupValueHandlers(updatePreview);
	$('#r1').keyup(updatePreview);
	$('#r2').keyup(updatePreview);
    }

    var superSetupFocusHandlers = that.setupFocusHandlers;
    that.setupFocusHandlers = function() {
	superSetupFocusHandlers();

	$('#r1').focus(function() { my.focussed = 'r1'; });
	$('#r1').blur(function()  { my.focussed = undefined; });

	$('#r2').focus(function() { my.focussed = 'r2'; });
	$('#r2').blur(function()  { my.focussed = undefined; });
    }

    var superCreate = that.create;
    that.create = function() {
	superCreate();
	$('#r1').focus();
	updatePreview();
    }

    return that;
}

SS.constructors.translate = function(spec) {

    var my = {};
    var geomNode = spec.geomNode;
    var geometry = sceneView.createGeometry(geomNode.mesh);
    var that = SS.constructors.origin(my);

    var superUpdatePreview = that.updatePreview;
    var updatePreview = function() {

	superUpdatePreview();

	var x = parseFloat($('#x').val()) || 0.0;
	var y = parseFloat($('#y').val()) || 0.0;
	var z = parseFloat($('#z').val()) || 0.0;
	var u = parseFloat($('#u').val()) || 0;
	var v = parseFloat($('#v').val()) || 0;
	var w = parseFloat($('#w').val()) || 0;

	var materials = [SS.constructors.faceMaterial];
	var mesh = new THREE.Mesh(geometry, materials);
	mesh.position.x = u - x;
	mesh.position.y = v - y;
	mesh.position.z = w - z;
	mesh.doubleSided = true;
	my.previewGeometry.addChild(mesh);

	my.previewGeometry.position.x = x;
	my.previewGeometry.position.y = y;
	my.previewGeometry.position.z = z;

	sceneView.scene.addObject(my.previewGeometry);
    }
    my.updatePreview = updatePreview;

    var superOnWorkplaneCursorUpdated = that.onWorkplaneCursorUpdated;
    that.onWorkplaneCursorUpdated = function(event) {
	
	if (superOnWorkplaneCursorUpdated(event)) {
	    updatePreview();
	} else if ((my.focussed === 'u') || (my.focussed === 'v') || (my.focussed === 'w')) {
	    var originX = parseFloat($('#x').val()), originY = parseFloat($('#y').val());
	    var x = event.x, y = event.y;
	    var u = x - originX, v = y - originY, w = 0;
	    
	    $('#u').val(u.toFixed(2));
	    $('#v').val(v.toFixed(2));
	    $('#w').val(w.toFixed(2));
	    updatePreview();

	} 
    }

    var superOnWorkplaneClicked = that.onWorkplaneClicked;
    that.onWorkplaneClicked = function(event) {
	superOnWorkplaneClicked({origin: 'u', u:'v', v:'w'});
    }
    
    var superSetupValueHandlers = that.setupValueHandlers;
    that.setupValueHandlers = function() {
	superSetupValueHandlers(updatePreview);
	$('#u').keyup(updatePreview);
	$('#v').keyup(updatePreview);
	$('#w').keyup(updatePreview);
    }

    var superSetupFocusHandlers = that.setupFocusHandlers;
    that.setupFocusHandlers = function() {
	superSetupFocusHandlers();

	$('#u').focus(function() { my.focussed = 'u'; });
	$('#u').blur(function()  { my.focussed = undefined; });

	$('#v').focus(function() { my.focussed = 'v'; });
	$('#v').blur(function()  { my.focussed = undefined; });

	$('#w').focus(function() { my.focussed = 'w'; });
	$('#w').blur(function()  { my.focussed = undefined; });
    }

    var superCreate = that.create;
    that.create = function() {
	superCreate();
	$('#u').focus();
	$('#n').val(0);
	updatePreview();
    }

    return that;
}

SS.constructors.scale = function(spec) {

    var my = {};
    var geomNode = spec.geomNode;
    var geometry = sceneView.createGeometry(geomNode.mesh);
    var originalPositions = geometry.vertices.map(function(vertex) {
	return vertex.position;
    });
    var that = SS.constructors.origin(my);

    var superUpdatePreview = that.updatePreview;
    var updatePreview = function() {

	superUpdatePreview();

	var x = parseFloat($('#x').val()) || 0.0;
	var y = parseFloat($('#y').val()) || 0.0;
	var z = parseFloat($('#z').val()) || 0.0;
	var factor = parseFloat($('#factor').val()) || 1.0;

	var scaledGeometry = sceneView.createGeometry(geomNode.mesh);
	scaledGeometry.vertices = originalPositions.map(function(position) {
	    var position = position.clone();
	    position.x = (position.x - x)*factor;
	    position.y = (position.y - y)*factor;
	    position.z = (position.z - z)*factor;
	    return new THREE.Vertex(position);
	});
	scaledGeometry.computeCentroids();
	scaledGeometry.computeFaceNormals();

	var materials = [SS.constructors.faceMaterial];
	var mesh = new THREE.Mesh(scaledGeometry, materials);

	mesh.doubleSided = true;
	my.previewGeometry.addChild(mesh);

	my.previewGeometry.position.x = x;
	my.previewGeometry.position.y = y;
	my.previewGeometry.position.z = z;

	sceneView.scene.addObject(my.previewGeometry);
    }
    my.updatePreview = updatePreview;

    var superOnWorkplaneCursorUpdated = that.onWorkplaneCursorUpdated;
    that.onWorkplaneCursorUpdated = function(event) {
	
	if (superOnWorkplaneCursorUpdated(event)) {
	    updatePreview();
	} 
    }

    var superOnWorkplaneClicked = that.onWorkplaneClicked;
    that.onWorkplaneClicked = function(event) {
	superOnWorkplaneClicked({});
    }
    
    var superSetupValueHandlers = that.setupValueHandlers;
    that.setupValueHandlers = function() {
	superSetupValueHandlers(updatePreview);
	$('#factor').keyup(updatePreview);
    }

    var superSetupFocusHandlers = that.setupFocusHandlers;
    that.setupFocusHandlers = function() {
	superSetupFocusHandlers();
    }

    var superCreate = that.create;
    that.create = function() {
	superCreate();
	$('#factor').val(1.0);
	updatePreview();
    }

    return that;
}

SS.constructors.rotate = function(spec) {

    var my = {};
    var geomNode = spec.geomNode;
    var geometry = sceneView.createGeometry(geomNode.mesh);
    var that = SS.constructors.origin(my);

    var superUpdatePreview = that.updatePreview;
    var updatePreview = function() {

	superUpdatePreview();

	var x = parseFloat($('#x').val()) || 0.0;
	var y = parseFloat($('#y').val()) || 0.0;
	var z = parseFloat($('#z').val()) || 0.0;
	var u = parseFloat($('#u').val()) || 0.0;
	var v = parseFloat($('#v').val()) || 0.0;
	var w = parseFloat($('#w').val()) || 0.0;
	var angle = parseFloat($('#angle').val()) || 0;
	
	var rotationVector = new THREE.Vector3(u, v, w).normalize();

	var axisGeom = new THREE.Geometry();
	var endPosition = rotationVector.clone().multiplyScalar(50);
	axisGeom.vertices.push(new THREE.Vertex(new THREE.Vector3()));
	axisGeom.vertices.push(new THREE.Vertex(endPosition));
	var axis = new THREE.Line(axisGeom, SS.constructors.lineMaterial);

	my.previewGeometry.addChild(axis);

	var coneGeometry =  new THREE.CylinderGeometry(50, 0, 2, 5); 
	var cone = new THREE.Mesh(coneGeometry, SS.constructors.solidFaceMaterial);
	cone.doubleSided = true;
	cone.position = endPosition;
	cone.lookAt(rotationVector);

	my.previewGeometry.addChild( cone );

	my.previewGeometry.position.x = x;
	my.previewGeometry.position.y = y;
	my.previewGeometry.position.z = z;

	sceneView.scene.addObject(my.previewGeometry);
    }
    my.updatePreview = updatePreview;

    var superOnWorkplaneCursorUpdated = that.onWorkplaneCursorUpdated;
    that.onWorkplaneCursorUpdated = function(event) {
	if (superOnWorkplaneCursorUpdated(event)) {
	    updatePreview();
	} 
    }

    var superOnWorkplaneClicked = that.onWorkplaneClicked;
    that.onWorkplaneClicked = function(event) {
	superOnWorkplaneClicked({origin: 'u', u:'v', v:'w', w:'angle'});
    }
    
    var superSetupValueHandlers = that.setupValueHandlers;
    that.setupValueHandlers = function() {
	superSetupValueHandlers(updatePreview);
	$('#u').keyup(updatePreview);
	$('#v').keyup(updatePreview);
	$('#w').keyup(updatePreview);
	$('#angle').keyup(updatePreview);
    }

    var superSetupFocusHandlers = that.setupFocusHandlers;
    that.setupFocusHandlers = function() {
	superSetupFocusHandlers();

	$('#u').focus(function() { my.focussed = 'u'; });
	$('#u').blur(function()  { my.focussed = undefined; });

	$('#v').focus(function() { my.focussed = 'v'; });
	$('#v').blur(function()  { my.focussed = undefined; });

	$('#w').focus(function() { my.focussed = 'w'; });
	$('#w').blur(function()  { my.focussed = undefined; });
	
	$('#angle').focus(function() { my.focussed = 'angle'; });
	$('#angle').blur(function()  { my.focussed = undefined; });
    }

    var superCreate = that.create;
    that.create = function() {
	superCreate();
	$('#x').focus();
	$('#u').val(0);
	$('#v').val(0);
	$('#w').val(1.0);
	$('#n').val(0);
	$('#angle').val(0);
	updatePreview();
    }

    return that;
}

SS.constructors.mirror = function(spec) {

    var my = {};
    var geomNode = spec.geomNode;
    var geometry = sceneView.createGeometry(geomNode.mesh);
    var that = SS.constructors.origin(my);

    var superUpdatePreview = that.updatePreview;
    var updatePreview = function() {

	superUpdatePreview();

	var x = parseFloat($('#x').val()) || 0.0;
	var y = parseFloat($('#y').val()) || 0.0;
	var z = parseFloat($('#z').val()) || 0.0;
	var u = parseFloat($('#u').val()) || 0.0;
	var v = parseFloat($('#v').val()) || 0.0;
	var w = parseFloat($('#w').val()) || 0.0;
	
	var rotationVector = new THREE.Vector3(u, v, w).normalize();

	var axisGeom = new THREE.Geometry();
	var endPosition = rotationVector.clone().multiplyScalar(50);
	axisGeom.vertices.push(new THREE.Vertex(new THREE.Vector3()));
	axisGeom.vertices.push(new THREE.Vertex(endPosition));
	var axis = new THREE.Line(axisGeom, SS.constructors.lineMaterial);

	my.previewGeometry.addChild(axis);

	var coneGeometry =  new THREE.CylinderGeometry(50, 0, 2, 5); 
	var cone = new THREE.Mesh(coneGeometry, SS.constructors.solidFaceMaterial);
	cone.doubleSided = true;
	cone.position = endPosition;
	cone.lookAt(rotationVector);

	my.previewGeometry.addChild( cone );

	my.previewGeometry.position.x = x;
	my.previewGeometry.position.y = y;
	my.previewGeometry.position.z = z;

	sceneView.scene.addObject(my.previewGeometry);
    }
    my.updatePreview = updatePreview;

    var superOnWorkplaneCursorUpdated = that.onWorkplaneCursorUpdated;
    that.onWorkplaneCursorUpdated = function(event) {
	if (superOnWorkplaneCursorUpdated(event)) {
	    updatePreview();
	} 
    }

    var superOnWorkplaneClicked = that.onWorkplaneClicked;
    that.onWorkplaneClicked = function(event) {
	superOnWorkplaneClicked({origin: 'u', u:'v', v:'w', w:'angle'});
    }
    
    var superSetupValueHandlers = that.setupValueHandlers;
    that.setupValueHandlers = function() {
	superSetupValueHandlers(updatePreview);
	$('#u').keyup(updatePreview);
	$('#v').keyup(updatePreview);
	$('#w').keyup(updatePreview);
    }

    var superSetupFocusHandlers = that.setupFocusHandlers;
    that.setupFocusHandlers = function() {
	superSetupFocusHandlers();

	$('#u').focus(function() { my.focussed = 'u'; });
	$('#u').blur(function()  { my.focussed = undefined; });

	$('#v').focus(function() { my.focussed = 'v'; });
	$('#v').blur(function()  { my.focussed = undefined; });

	$('#w').focus(function() { my.focussed = 'w'; });
	$('#w').blur(function()  { my.focussed = undefined; });
    }

    var superCreate = that.create;
    that.create = function() {
	superCreate();
	$('#x').focus();
	$('#u').val(0);
	$('#v').val(0);
	$('#w').val(1.0);
	$('#n').val(0);
	updatePreview();
    }

    return that;
}