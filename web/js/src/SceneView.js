var SS = SS || {};
SS.SceneView = function(container) {

    var camera, scene, renderer, w, h;
    var overRenderer;

    var mouseOnDown = null;
    var lastMouseDownTime = null;
    var popupMenuDelay = 200;
    var showPopup = false, showingPopup = false;
    var planeMesh, mouseOnWorkplane = {x: 0, y: 0}, workplaneXObj, workplaneYObj, workplanePointer;
        
    var elevation = 0, azimuth = Math.PI/4;
    var target = { azimuth: Math.PI/4, elevation: Math.PI*3/8 };
    var targetOnDown = { azimuth: target.azimuth, elevation: target.elevation };
    var distance = 1000, distanceTarget = 400;

    var pathToModel = {};
    var unselectedColor = 0x00dd00, selectedColor = 0xdddd00;
    var panning = false, rotating = false, threshhold = 10; // inside this threshold is a single click
    
    function init() {

	container.style.color = '#fff';
	container.style.font = '13px/20px Arial, sans-serif';

	var shader, uniforms, material;
	w = container.offsetWidth || window.innerWidth;
	h = container.offsetHeight || window.innerHeight;

	camera = new THREE.Camera(30, w / h, 1, 10000);
	camera.up.x = 0;
	camera.up.y = 0;
	camera.up.z = 1;
	
	vector = new THREE.Vector3();
	scene = new THREE.Scene();

	addGrid();

	ambientLight = new THREE.AmbientLight( 0x101010 );
	scene.addLight( ambientLight );

	pointLight = new THREE.PointLight( 0xa0a050 );
	pointLight.position.z = 100;
	scene.addLight(pointLight);

	pointLight = new THREE.PointLight( 0x333333 );
	pointLight.position.z = -100;
	scene.addLight(pointLight);

	pointLight = new THREE.PointLight( 0x333333 );
	pointLight.position.x = -100;
	pointLight.position.y = -100;
	scene.addLight(pointLight);

	directionalLight = new THREE.DirectionalLight( 0xaaaa88 );
	directionalLight.position.x = 100;
	directionalLight.position.y = 50;
	directionalLight.position.z = 50;
	directionalLight.position.normalize();
	scene.addLight( directionalLight );

	renderer = new THREE.WebGLRenderer({antialias: true});
	renderer.autoClear = false;
	renderer.setClearColorHex(0x080808, 0.0);
	renderer.setSize(w, h);

	renderer.domElement.style.position = 'absolute';

	container.appendChild(renderer.domElement);

	container.addEventListener('mousedown', onMouseDown, false);
	container.addEventListener('mousewheel', onMouseWheel, false);
	container.addEventListener('mousemove', onMouseMove, false);
	document.addEventListener('keydown', onDocumentKeyDown, false);
	window.addEventListener('resize', onWindowResize, false);
	container.addEventListener('mouseover', function() {
	    overRenderer = true;
	}, false);
	container.addEventListener('mouseout', function() {
	    overRenderer = false;
	}, false);
    }


    function onMouseDown(event) {
	event.preventDefault();

	showPopup = true;
	showingPopup = false;
	lastMouseDownTime = new Date().getTime();
	setTimeout(function() { popupMenu(); }, popupMenuDelay);
	mouseOnDown = {};
	mouseOnDown.x = event.clientX;
	mouseOnDown.y = event.clientY;

	rotating = false;
	panning = false;
	
	container.addEventListener('mouseup', onMouseUp, false);
	//container.addEventListener('mouseout', onMouseOut, false);

    }
    
    function onMouseMove(event) {

	if (mouseOnDown && !showingPopup) {
	    if (!(rotating || panning)) {
		if (event.button == 0 && !event.shiftKey) {
		    if ((Math.abs(event.clientX - mouseOnDown.x) > threshhold)
			||
			(Math.abs(event.clientY - mouseOnDown.y) > threshhold)) {

			rotating = true;
		    }
		} 
		if (event.button == 1 || event.shiftKey) {
		    if ((Math.abs(event.clientX - mouseOnDown.x) > threshhold)
			||
			(Math.abs(event.clientY - mouseOnDown.y) > threshhold)) {
			panning = true;
		    }
		}
	    }

	    if (panning || rotating) {
		container.style.cursor = 'move';
	    } else {
		showPopup = false;
	    }

	    if (rotating) {
		var mouse = {};
		mouse.x = event.clientX;
		mouse.y = event.clientY;
		var zoomDamp = Math.sqrt(distance)/10;

		target.azimuth = targetOnDown.azimuth - (mouse.x - mouseOnDown.x) * 0.005 * zoomDamp;
		target.elevation = targetOnDown.elevation - (mouse.y - mouseOnDown.y) * 0.005 * zoomDamp;

		target.elevation = target.elevation > Math.PI ? Math.PI : target.elevation;
		target.elevation = target.elevation < 0 ? 0 : target.elevation;
	    }
	} else if (!showingPopup) {

	    var mouse = {};
	    mouse.x = ( event.clientX / window.innerWidth ) * 2 - 1;
	    mouse.y = - ( event.clientY / window.innerHeight ) * 2 + 1;
	    
	    var vector = new THREE.Vector3( mouse.x, mouse.y, 0.5 );
	    var projector = new THREE.Projector();
	    var mouse3D = projector.unprojectVector(vector, camera);
	    var ray = new THREE.Ray(camera.position, null);
	    ray.direction = mouse3D.subSelf(camera.position).normalize();
	    var intersects = ray.intersectObject(planeMesh);
	    if (intersects.length == 1) {
		updateWorkplaneXYLocation(intersects[0].point.x, intersects[0].point.y);
	    }
	    
	}
    }

    function popupMenu() {
	if (showPopup) {
	    document.getElementById('toolWheel').addEventListener('mouseup', onMouseUp, false);
	    showingPopup = true;
	    $('#toolWheel').css('left', mouseOnDown.x);
	    $('#toolWheel').css('top', mouseOnDown.y);

	    if (selectionManager.selected().length == 0) {
		$('#toolWheel').append($('#primitives'));
	    } else if (selectionManager.selected().length == 1) {
		$('#toolWheel').append($('#edit'));
		$('#toolWheel').append($('#transforms'));
		$('#toolWheel').append($('#copyTransforms'));
	    } else if (selectionManager.selected().length == 2) {
		$('#toolWheel').append($('#boolean'));
	    }

	    $('#toolWheel').show();
	}
    }

    function onMouseUp(event) {

	targetOnDown.azimuth = target.azimuth;
	targetOnDown.elevation = target.elevation;
	showPopup = false;

	if (!panning && !rotating) {

	    var now = new Date().getTime();
	    console.log(now - lastMouseDownTime);
	    if (now - lastMouseDownTime < popupMenuDelay) {

		var mouse = {};
		mouse.x = ( event.clientX / window.innerWidth ) * 2 - 1;
		mouse.y = - ( event.clientY / window.innerHeight ) * 2 + 1;

		var vector = new THREE.Vector3( mouse.x, mouse.y, 0.5 );
		var projector = new THREE.Projector();
		var mouse3D = projector.unprojectVector(vector, camera);
		var ray = new THREE.Ray(camera.position, null);
		ray.direction = mouse3D.subSelf(camera.position).normalize();
		var intersects = ray.intersectScene(scene);

		var foundObjectPath = null;
		if (intersects.length > 0) {
		    for (var i in intersects) {
			if (intersects[i].object.name) {
			    foundObjectPath = intersects[i].object.name;
			    break;
			}
		    }
		}

		if (foundObjectPath) {
		    if (event.shiftKey) {
			selectionManager.shiftPick(foundObjectPath);
		    } else {
			selectionManager.pick(foundObjectPath);
		    }
		} else {
		    selectionManager.deselectAll();
		}
	    }
	}

	rotating = false;
	panning = false;
	if (showingPopup) {
	    showingPopup = false;
	    $('#toolWheel').hide();
	    var toolbars = $('#toolWheel').children().detach();
	    $('#toolbarStaging').append(toolbars);
	    document.getElementById('toolWheel').removeEventListener('mouseup', onMouseUp, false);
	}

	mouseOnDown = null;
	container.removeEventListener('mouseup', onMouseUp, false);
	//container.removeEventListener('mouseout', onMouseOut, false);
	container.style.cursor = 'auto';
    }

    function onMouseOut(event) {
	mouseOnDown = null;
	container.removeEventListener('mouseup', onMouseUp, false);
	//container.removeEventListener('mouseout', onMouseOut, false);
    }

    function onMouseWheel(event) {
	event.preventDefault();
	if (overRenderer) {
	    zoom(event.wheelDeltaY * 0.05);
	}
	return false;
    }

    function onDocumentKeyDown(event) {
	switch (event.keyCode) {
	case 38:
            zoom(100);
            event.preventDefault();
            break;
	case 40:
            zoom(-100);
            event.preventDefault();
            break;
	}
    }

    function onWindowResize(event) {
	console.log('resize');
	camera.aspect = window.innerWidth / window.innerHeight;
	camera.updateProjectionMatrix();
	renderer.setSize(window.innerWidth, window.innerHeight);
    }

    function zoom(delta) {
	distanceTarget -= delta;
    }

    function animate() {
	requestAnimationFrame(animate);
	render();
    }

    function render() {
	zoom(0);

	azimuth += (target.azimuth - azimuth) * 0.2;
	elevation += (target.elevation - elevation) * 0.3;

	var dDistance = (distanceTarget - distance) * 0.3;

	if (distance + dDistance > 1000) {
	    distanceTarget = 1000;
	    distance = 1000;
	} else if (distance + dDistance < 3) {
	    distanceTarget = 3;
	    distance = 3;
	} else {
	    distance += dDistance;
	}

	camera.position.x = distance * Math.sin(elevation) * Math.cos(azimuth);
	camera.position.y = distance * Math.sin(elevation) * Math.sin(azimuth);
	camera.position.z = distance * Math.cos(elevation);

	renderer.clear();
	renderer.render(scene, camera);
    }
    
    var insideX = [-50,50];
    var insideY = [-50,50];
    var gridExtents = 100;
    var majorTick = 10;

    function addGrid() {

	var height = 0.01,
	size = 2,
	curveSegments = 6,
	font = "helvetiker", 		
	weight = "bold",		
	style = "normal";

	var textMaterial = new THREE.MeshBasicMaterial( { color: 0xffffff, opacity: 0.5, wireframe: false } );

	for (var x = (insideX[0]/majorTick); x <= (insideX[1]/majorTick); ++x) {
	    var textGeo = new THREE.TextGeometry( '' + (1.0*x*10), {
		size: size, 
		height: height,
		curveSegments: curveSegments,
		font: font,
		weight: weight,
		style: style,
		bezelEnabled: false
	    });
	    var textMesh1 = new THREE.Mesh( textGeo, textMaterial );
	    textMesh1.position.y = insideY[1] + 3;
	    textMesh1.position.x = x*10 + (x > 0 ? -2 : (x < 0 ? 2.5 : 0));
	    textMesh1.rotation.z = Math.PI;
	    scene.addObject(textMesh1);
	}

	for (var y = (insideY[0]/majorTick); y <= (insideY[1]/majorTick); ++y) {
	    var textGeo = new THREE.TextGeometry( '' + (1.0*y*10), {
		size: size, 
		height: height,
		curveSegments: curveSegments,
		font: font,
		weight: weight,
		style: style,
		bezelEnabled: false
	    });
	    var textMesh1 = new THREE.Mesh( textGeo, textMaterial );
	    textMesh1.position.y = y*10 + (y > 0 ? -2 : (y < 0 ? 2.5 : 0));
	    textMesh1.position.x = insideX[1] + 3;
	    textMesh1.rotation.z = Math.PI/2;
	    scene.addObject(textMesh1);
	}

	addPlane();
	addAxes();
	addMainGrid();
	for(var x = -gridExtents; x <= gridExtents; ++x) {
	    for(var y = -gridExtents; y <= gridExtents; ++y) {
		var inside = ((x >= insideX[0]) && (x <= insideX[1]) &&
			      (y >= insideY[0]) && (y <= insideY[1]));
		if ((x % majorTick == 0) && (y % majorTick == 0) &&
		    (x != 0) && (y != 0) &&
		    !inside) {
		    addFadingGridTile(x,y);
		}
	    }
	}

    }

    function updateWorkplaneXYLocation(x, y) {

	var x = Math.round(x);
	var y = Math.round(y);
	var isInsideX = (x >= insideX[0]) && (x <= insideX[1]);
	var isInsideY = (y >= insideY[0]) && (y <= insideY[1]);

	if (workplaneXObj) {
	    scene.removeObject(workplaneXObj);
	}
	if (workplaneYObj) {
	    scene.removeObject(workplaneYObj);
	}
	if (workplanePointer) {
	    scene.removeObject(workplanePointer);
	}

	if (isInsideX) {
	    updateWorkplaneX(x);
	}
	if (isInsideY) {
	    updateWorkplaneY(y);
	}
	if (isInsideX && isInsideY) {
	    updateWorkplanePointer(x,y);
	}
    }
    
    function labelMesh(value) {
	var height = 0.01,
	size = 2,
	curveSegments = 6,
	font = "helvetiker", 		
	weight = "bold",		
	style = "normal";
	var labelMaterial = new THREE.MeshBasicMaterial( { color: 0xffff00, opacity: 0.7, wireframe: false } );
	var labelGeometry = new THREE.TextGeometry('' + Math.round(value), {
	    size: size, 
	    height: height,
	    curveSegments: curveSegments,
	    font: font,
	    weight: weight,
	    style: style,
	    bezelEnabled: false
	});
	return new THREE.Mesh(labelGeometry, labelMaterial);
    }

    function updateWorkplanePointer(x,y) {

	var pointerMaterial = new THREE.MeshBasicMaterial( { color: 0xffff00, opacity: 0.7, wireframe: false } );
	var pointerGeometry = new THREE.CubeGeometry(0.5, 0.5, 0.5);
	workplanePointer = new THREE.Mesh(pointerGeometry, pointerMaterial);
	workplanePointer.position.x = x;
	workplanePointer.position.y = y;
	scene.addObject(workplanePointer);

    }

    function updateWorkplaneX(x) {

	workplaneXObj = new THREE.Object3D();
	var background = new THREE.Mesh(new THREE.PlaneGeometry(5, 3),
					new THREE.MeshBasicMaterial({ color: 0x101010, opacity: 1.0 }));
	background.position.x = x;
	background.position.y = insideY[1] + 2;
	background.position.z = 0.1;

	var borderGeom = [new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry()];
	borderGeom[0].vertices.push(new THREE.Vertex(new THREE.Vector3(-2.5, 0, 0)));
	borderGeom[0].vertices.push(new THREE.Vertex(new THREE.Vector3(2.5, 0, 0)));
	borderGeom[1].vertices.push(new THREE.Vertex(new THREE.Vector3(2.5, 0, 0)));
	borderGeom[1].vertices.push(new THREE.Vertex(new THREE.Vector3(2.5, 3, 0)));
	borderGeom[2].vertices.push(new THREE.Vertex(new THREE.Vector3(2.5, 3, 0)));
	borderGeom[2].vertices.push(new THREE.Vertex(new THREE.Vector3(-2.5, 3, 0)));
	borderGeom[3].vertices.push(new THREE.Vertex(new THREE.Vector3(-2.5, 3, 0)));
	borderGeom[3].vertices.push(new THREE.Vertex(new THREE.Vector3(-2.5, 0, 0)));
	for (var i = 0; i < 4; ++i) {
	    var border = new THREE.Line(borderGeom[i], new THREE.LineBasicMaterial({ color: 0xe9fb00, opacity: 0.5 }));
	    border.position.x = x;
	    border.position.y = insideY[1] + 0.5;
	    border.position.z = 0.15;
	    workplaneXObj.addChild(border);
	}

	var arrowGeometry = new THREE.Geometry();
	arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(-0.5, 0.5, 0)));
	arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0.5, 0.5, 0)));
	var face = new THREE.Face3(0,1,2);
	arrowGeometry.faces.push(face);
	arrowGeometry.computeCentroids();
	arrowGeometry.computeFaceNormals();

	var arrowMaterial = new THREE.MeshBasicMaterial({ color: 0xe9fb00, opacity: 1.0 });
	var arrow = new THREE.Mesh(arrowGeometry, arrowMaterial);
	arrow.doubleSided = true;
	arrow.position.x = x;
	arrow.position.y = insideY[1];
	arrow.position.z = 0.15;
	workplaneXObj.addChild(arrow);

	var label = labelMesh(x);
	label.position.x = x;
	label.position.y = insideY[1] + 3;
	label.position.z = 0.1;
	label.rotation.z = Math.PI;

	workplaneXObj.addChild(background);
	workplaneXObj.addChild(label);
	scene.addObject(workplaneXObj);
    }

    function updateWorkplaneY(y) {

	workplaneYObj = new THREE.Object3D();
	var background = new THREE.Mesh(new THREE.PlaneGeometry(3, 5),
					new THREE.MeshBasicMaterial({ color: 0x101010, opacity: 1.0 }));
	background.position.x = insideX[1] + 2;
	background.position.y = y;
	background.position.z = 0.1;

	var borderGeom = [new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry()];
	borderGeom[0].vertices.push(new THREE.Vertex(new THREE.Vector3(-2.5, 0, 0)));
	borderGeom[0].vertices.push(new THREE.Vertex(new THREE.Vector3(2.5, 0, 0)));
	borderGeom[1].vertices.push(new THREE.Vertex(new THREE.Vector3(2.5, 0, 0)));
	borderGeom[1].vertices.push(new THREE.Vertex(new THREE.Vector3(2.5, 3, 0)));
	borderGeom[2].vertices.push(new THREE.Vertex(new THREE.Vector3(2.5, 3, 0)));
	borderGeom[2].vertices.push(new THREE.Vertex(new THREE.Vector3(-2.5, 3, 0)));
	borderGeom[3].vertices.push(new THREE.Vertex(new THREE.Vector3(-2.5, 3, 0)));
	borderGeom[3].vertices.push(new THREE.Vertex(new THREE.Vector3(-2.5, 0, 0)));
	for (var i = 0; i < 4; ++i) {
	    var border = new THREE.Line(borderGeom[i], new THREE.LineBasicMaterial({ color: 0xe9fb00, opacity: 0.5 }));
	    border.position.x = insideX[1] + 0.5;
	    border.position.y = y;
	    border.position.z = 0.15;
	    border.rotation.z = Math.PI*3/2;
	    workplaneYObj.addChild(border);
	}

	var arrowGeometry = new THREE.Geometry();
	arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(-0.5, 0.5, 0)));
	arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0.5, 0.5, 0)));
	var face = new THREE.Face3(0,1,2);
	arrowGeometry.faces.push(face);
	arrowGeometry.computeCentroids();
	arrowGeometry.computeFaceNormals();

	var arrowMaterial = new THREE.MeshBasicMaterial({ color: 0xe9fb00, opacity: 1.0 });
	var arrow = new THREE.Mesh(arrowGeometry, arrowMaterial);
	arrow.doubleSided = true;
	arrow.position.x = insideX[1];
	arrow.position.y = y;
	arrow.position.z = 0.15;
	arrow.rotation.z = Math.PI*3/2;
	workplaneYObj.addChild(arrow);

	var label = labelMesh(y);
	label.position.x = insideX[1] + 3;
	label.position.y = y;
	label.position.z = 0.1;
	label.rotation.z = Math.PI/2;

	workplaneYObj.addChild(background);
	workplaneYObj.addChild(label);
	scene.addObject(workplaneYObj);
    }

    function addPlane() {
	var planeGeometry = new THREE.PlaneGeometry(1000, 1000);
	planeMesh = new THREE.Mesh(planeGeometry,
				   new THREE.MeshBasicMaterial({ color: 0x080808, opacity: 0 }));
	planeMesh.doubleSided = true;
	scene.addObject(planeMesh);
    }

    function addAxes() {

	axes = [new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry()];
	axes[0].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[0].vertices.push(new THREE.Vertex(new THREE.Vector3(500, 0, 0)));

	axes[1].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[1].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 500, 0)));

	axes[2].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[2].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 500)));

	axes[3].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[3].vertices.push(new THREE.Vertex(new THREE.Vector3(-gridExtents, 0, 0)));

	axes[4].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[4].vertices.push(new THREE.Vertex(new THREE.Vector3(0, -gridExtents, 0)));

	scene.addObject(new THREE.Line(axes[0], new THREE.LineBasicMaterial({ color: 0x0000ff, opacity: 0.5 }))); //  X
	scene.addObject(new THREE.Line(axes[1], new THREE.LineBasicMaterial({ color: 0x00ff00, opacity: 0.5 }))); //  Y
	scene.addObject(new THREE.Line(axes[2], new THREE.LineBasicMaterial({ color: 0xff0000, opacity: 0.5 }))); //  Z
	scene.addObject(new THREE.Line(axes[3], new THREE.LineBasicMaterial({ color: 0x0000ff, opacity: 0.2 }))); // -X
	scene.addObject(new THREE.Line(axes[4], new THREE.LineBasicMaterial({ color: 0x00ff00, opacity: 0.2 }))); // -X
    }

    var majorGridLineGeometry = new THREE.Geometry();
    majorGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(insideX[0], 0, 0)));
    majorGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(insideX[1], 0, 0)));

    var minorGridLineGeometry = new THREE.Geometry();
    minorGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(insideX[0], 0, 0)));
    minorGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(insideX[1], 0, 0)));

    var majorMaterialInside = new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.5 });
    var minorMaterialInside = new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.05 });

    function addMainGrid() {

	for (var x = insideX[0]; x <= insideX[1]; ++x) {
	    if (x != 0) {
		var material = (x % 10 == 0) ? majorMaterialInside : minorMaterialInside;
		var geometry = (y % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
		var line = new THREE.Line(geometry, material);
		line.position.x = x;
		line.rotation.z = 90 * Math.PI / 180;
		scene.addObject(line);

	    }
	}

	for (var y = insideY[0]; y <= insideY[1]; ++y) {
	    if (y != 0) {
		var material = (y % 10 == 0) ? majorMaterialInside : minorMaterialInside;
		var geometry = (y % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
		var line = new THREE.Line(geometry, material);
		line.position.y = y;
		scene.addObject(line);
	    }
	}
    }
    
    var fadingGridLineGeometry = new THREE.Geometry();
    fadingGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
    fadingGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(majorTick, 0, 0)));
    
    function addFadingGridTile(x,y) {
	
	var dx = 0;
	if (x < insideX[0]) {
	    dx = insideX[0] - x;
	} else if (x > insideX[1]) {
	    dx = x - insideX[1];
	}
	var dy = 0;
	if (y < insideY[0]) {
	    dy = insideY[0] - y;
	} else if (y > insideY[1]) {
	    dy = y - insideY[1];
	}

	var r = Math.sqrt(dx*dx + dy*dy);
	var opacity = (r == 0) ? 1.0 : 1.0/(r*0.9);
	var material = new THREE.LineBasicMaterial({ color: 0xffffff, opacity: opacity });

	var line = new THREE.Line(fadingGridLineGeometry, material);
	line.position.x = x > 0 ? (x-majorTick) : x;
	line.position.y = y;
	scene.addObject(line);
	
	var line = new THREE.Line(fadingGridLineGeometry, material);
	line.position.x = x;
	line.position.y = y > 0 ? (y-majorTick) : y;
	line.rotation.z = 90 * Math.PI / 180;
	scene.addObject(line);

    }

    this.geomDocUpdated = function(event) {

        if (event.add) {
            add(event.add);
        }

        if (event.remove) {
            remove(event.remove);
        }

        if (event.replace) {
            remove(event.replace.original);
            add(event.replace.replacement);
        }
    }

    var add = function(geomNode) {
        if (geom_doc.isRoot(geomNode) && geomNode.mesh) {
	    var geometry = createGeometry(geomNode.mesh);
	    var material = new THREE.MeshPhongMaterial( { ambient: 0x030303, color: unselectedColor, specular: 0xccffcc, shininess: 100, shading: THREE.SmoothShading } );
	    var mesh = new THREE.Mesh(geometry, material);
	    mesh.doubleSided = true;
	    mesh.name = geomNode.path;
	    scene.addObject(mesh);
	    pathToModel[geomNode.path] = mesh;
        }
    }

    var remove = function(geomNode) {
        if (geomNode.path) {
	    var mesh = pathToModel[geomNode.path];
	    scene.removeObject(mesh);
	    delete pathToModel[geomNode.path];
        }
    }

    this.selectionUpdated = function(event) {
        if (event.deselected) {
            for (var i in event.deselected) {
                var path = event.deselected[i];
		pathToModel[path].materials[0].color.setHex(unselectedColor);
            }
        }
        if (event.selected) {
            for (var i in event.selected) {
                var path = event.selected[i];
		pathToModel[path].materials[0].color.setHex(selectedColor);
		
            }
        }
    }

    var createGeometry = function(mesh) {
	var geometry = new THREE.Geometry();

	for (var i = 0; i  < mesh.positions.length/3; ++i) {
	    var position = new THREE.Vector3(mesh.positions[i * 3], 
					     mesh.positions[i * 3 + 1], 
					     mesh.positions[i * 3 + 2]);
	    var vertex = new THREE.Vertex(position);
	    geometry.vertices.push(vertex);
	}
	
	for (var i = 0; i < mesh.indices.length/3; ++i) {
	    var a = mesh.indices[i * 3],
	    b = mesh.indices[i * 3 + 1],
	    c = mesh.indices[i * 3 + 2];

	    var face = new THREE.Face3(a,b,c);
	    face.vertexNormals = [new THREE.Vector3(mesh.normals[a*3], 
						    mesh.normals[a*3+1], 
						    mesh.normals[a*3+2]),
				  new THREE.Vector3(mesh.normals[b*3], 
						    mesh.normals[b*3+1], mesh.normals[b*3+2]),
				  new THREE.Vector3(mesh.normals[c*3], 
						    mesh.normals[c*3+1], mesh.normals[c*3+2])];
	    geometry.faces.push(face);
	}
	geometry.computeCentroids();
	geometry.computeFaceNormals();
	return geometry;
    }
    
    init();
    this.animate = animate;
    this.renderer = renderer;
    this.scene = scene;

    return this;
}
