var SS = SS || {};

SS.constructors = {};

SS.constructors.originMixin = function(shared) {
    var that = {};

    that.onWorkplaneCursorUpdated = function (event) {
	if (shared.focussed === 'origin') {
	    $('#x').val(event.x);
	    $('#y').val(event.y);
	    return true;
	}
	return false;
    }

    that.setupValueHandlers = function() {
	$('#x').change(shared.updatePreview);
	$('#y').change(shared.updatePreview);
	$('#z').change(shared.updatePreview);
    }

    that.setupFocusHandlers = function() {
	$('#x').focus(function() { shared.focussed = 'origin'; });
	$('#y').focus(function() { shared.focussed = 'origin'; });
	$('#z').focus(function() { shared.focussed = 'origin'; });

	$('#x').blur(function() { shared.focussed = undefined; });
	$('#y').blur(function() { shared.focussed = undefined; });
	$('#z').blur(function() { shared.focussed = undefined; });

	$('#modal-ok').focus(function() { shared.focussed = 'ok'; });
	$('#model-ok').blur(function()  { shared.focussed = undefined; });
    }


    that.updatePreview = function() {

	if (shared.previewGeometry) {
	    sceneView.scene.removeObject(shared.previewGeometry);
	}
	shared.previewGeometry = new THREE.Object3D();

	var pointerMaterial = new THREE.MeshBasicMaterial( { color: 0xffffff, opacity: 0.5, wireframe: false } );
	var pointerGeometry = new THREE.CubeGeometry(0.5, 0.5, 0.5);
	var pointer = new THREE.Mesh(pointerGeometry, pointerMaterial);
	

	shared.previewGeometry.addChild(pointer);

    }

    that.create = function() {
	var lastMousePosition = sceneView.workplane.getLastMousePosition();

	shared.setupValueHandlers();
	shared.setupFocusHandlers();

	var originX = lastMousePosition.x;
	var originY = lastMousePosition.y;
	$('#x').val(originX);
	$('#y').val(originY);

	sceneView.workplane.on('workplaneCursorUpdated', shared.onWorkplaneCursorUpdated);
	sceneView.workplane.on('workplaneClicked', shared.onWorkplaneClicked);

    }
    
    that.dispose = function() {
	sceneView.workplane.off('workplaneCursorUpdated', shared.onWorkplaneCursorUpdated);
	sceneView.workplane.off('workplaneClicked', shared.onWorkplaneClicked);
	
	if (shared.previewGeometry) {
	    sceneView.scene.removeObject(shared.previewGeometry);
	}
	SS.constructors.active = null;
    }

    return that;
}

SS.constructors.sphere = function() {
    var that = {};
    var shared = {}, originMixin; 

    shared.updatePreview = function() {
	originMixin.updatePreview();
	
	var x = parseFloat($('#x').val());
	var y = parseFloat($('#y').val());
	var r  =parseFloat($('#r').val());

	var geometry = new THREE.SphereGeometry(r, 50, 10);
	var material = new THREE.MeshBasicMaterial({color: 0x3F8FD2, opacity: 0.5});
	var sphere = new THREE.Mesh(geometry, material);
	shared.previewGeometry.addChild(sphere);

	var circleGeom = new THREE.Geometry();
	for(var i = 0; i <= 50; ++i) {
	    var theta = Math.PI*2*i/50;
	    var dx = r*Math.cos(theta);
	    var dy = r*Math.sin(theta);
	    circleGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	}
	var circle = new THREE.Line(circleGeom, new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.5 }));
	shared.previewGeometry.addChild(circle);
    
	shared.previewGeometry.position.x = x;
	shared.previewGeometry.position.y = y;
	sceneView.scene.addObject(shared.previewGeometry);
    }

    shared.onWorkplaneCursorUpdated = function(event) {

	if (originMixin.onWorkplaneCursorUpdated(event)) {
	    shared.updatePreview();
	} else if (shared.focussed === 'r') {
	    var originX = $('#x').val(), originY = $('#y').val();
	    var x = event.x, y = event.y;
	    
	    var dx = Math.abs(x - originX), dy = Math.abs(y - originY);
	    var r  = Math.sqrt(dx*dx + dy*dy);
	    
	    $('#r').val(r.toFixed(2));
	    shared.updatePreview();
	}
    }

    shared.onWorkplaneClicked = function(event) {
	if (shared.focussed === 'origin') {
	    $('#modal-ok').focus();
	} else if (shared.focussed === 'r') {
	    $('#modal-ok').focus();
	}
    }

    shared.setupValueHandlers = function() {
	originMixin.setupValueHandlers();
	$('#r').change(shared.updatePreview);
    }

    shared.setupFocusHandlers = function() {
	originMixin.setupFocusHandlers();
	$('#r').focus(function() { shared.focussed = 'r'; });
	$('#r').blur(function()  { shared.focussed = undefined; });
    }

    var originMixin = this.originMixin(shared);

    that.create = function() {
	originMixin.create();
	$('#r').focus();
	SS.constructors.active = this;
    }

    that.dispose = function() {
	originMixin.dispose();
    }
    
    return that;
}

SS.constructors.cylinder = function(spec) {
    var that = {};
    var shared = {}, originMixin;

    shared.updatePreview = function() {

	originMixin.updatePreview();

	var originAxisGeom = new THREE.Geometry();
	originAxisGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	originAxisGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 50)));

	var originAxis = new THREE.Line(originAxisGeom, new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.5 }));
	shared.previewGeometry.addChild(originAxis);

	var x = parseFloat($('#x').val());
	var y = parseFloat($('#y').val());
	var r = parseFloat($('#r').val());
	var h = parseFloat($('#h').val());

	if (h) {
	    var geometry = new THREE.CylinderGeometry(50, r, r, h);
	    var material = new THREE.MeshBasicMaterial({color: 0x3F8FD2, opacity: 0.5});
	    var cylinder = new THREE.Mesh(geometry, material);
	    cylinder.position.z = h/2;
	    shared.previewGeometry.addChild(cylinder);
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
	shared.previewGeometry.addChild(circle1);

	if (h) {
	    var circle2 = new THREE.Line(circleGeom2, new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.5 }));
	    circle2.position.z = h;
	    shared.previewGeometry.addChild(circle2);
	}

	shared.previewGeometry.position.x = x;
	shared.previewGeometry.position.y = y;

	sceneView.scene.addObject(shared.previewGeometry);
	
    }

    shared.onWorkplaneCursorUpdated = function(event) {
	
	if (originMixin.onWorkplaneCursorUpdated(event)) {
	    shared.updatePreview();
	} else if (shared.focussed === 'r') {
	    var originX = $('#x').val(), originY = $('#y').val();
	    var x = event.x, y = event.y, r;
	    
	    var dx = Math.abs(x - originX), dy = Math.abs(y - originY);
	    var r  = Math.sqrt(dx*dx + dy*dy);
	    
	    $('#r').val(r.toFixed(2));
	    shared.updatePreview();
	} else if (shared.focussed === 'h') {
	    shared.updatePreview();
	}

    }

    shared.onWorkplaneClicked = function(event) {
	if (shared.focussed === 'origin') {
	    if ($('#h').val() === '') {
		$('#h').focus();
	    } else {
		$('#modal-ok').focus();
	    }
	} else if (shared.focussed === 'r') {
	    if ($('#h').val() === '') {
		$('#h').focus();		
	    } else {
		$('#modal-ok').focus();
	    }
	} else if (shared.focussed === 'h') {
	    $('#modal-ok').focus();
	}
    }

    shared.setupValueHandlers = function() {
	originMixin.setupValueHandlers(shared.updatePreview);
	$('#r').change(shared.updatePreview);
	$('#h').change(shared.updatePreview);
    }

    shared.setupFocusHandlers = function() {
	originMixin.setupFocusHandlers();

	$('#r').focus(function() { shared.focussed = 'r'; });
	$('#r').blur(function()  { shared.focussed = undefined; });

	$('#h').focus(function() { shared.focussed = 'h'; });
	$('#h').blur(function()  { shared.focussed = undefined; });
    }

    var originMixin = this.originMixin(shared);
    
    that.create = function() {
	originMixin.create();
	$('#r').focus();
	SS.constructors.active = this;
    }

    that.dispose = function() {
	originMixin.dispose();
    }
    
    return that;
}

