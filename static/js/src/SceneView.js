var SS = SS || {};
SS.SceneView = function(container) {

    var camera, scene, renderer, w, h;
    var overRenderer;

    var mouseOnDown = null;
    var lastMouseDownTime = null;

    var elevation = 0, azimuth = Math.PI/4;
    var target = { azimuth: Math.PI/4, elevation: Math.PI*3/8 };
    var targetOnDown = { azimuth: target.azimuth, elevation: target.elevation };
    var distance = 1000, distanceTarget = 400;

    var idToModel = {};
    var unselectedColor = 0x00dd00, selectedColor = 0xdddd00;
    var panning = false, rotating = false, threshhold = 10; // inside this threshold is a single click

    var workplane;
    var popupMenu = SS.popupMenu();
    
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

	addLights();
	workplane = new SS.Workplane({scene: scene});
	popupMenu = SS.popupMenu();

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
	container.addEventListener('mouseover', function() {
	    overRenderer = true;
	}, false);
	container.addEventListener('mouseout', function() {
	    overRenderer = false;
	}, false);
	container.addEventListener('dblclick', function() {
	    popupMenu.cancel();
	    if (selectionManager.size() == 1) {
		var id = selectionManager.getSelected()[0];

		$('#' + id + ' > tbody > tr:nth-child(1)').addClass('selected');
		treeView.edit(id);
	    }
	    
	});

	window.addEventListener('resize', onWindowResize, false);
	
    }


    function onMouseDown(event) {
	event.preventDefault();

	popupMenu.onMouseDown(event);

	mouseOnDown = {};
	mouseOnDown.x = event.clientX;
	mouseOnDown.y = event.clientY;

	rotating = false;
	panning = false;



	container.addEventListener('mouseup', onMouseUp, false);


    }
    
    function onMouseMove(event) {

	if (mouseOnDown) {
	    if (!(rotating || panning) && !popupMenu.isShowing()) {
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
		popupMenu.cancel()
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
	} else {
	    var positionOnWorkplane = determinePositionOnWorkplane(event);
	    workplane.updateXYLocation(positionOnWorkplane, event);
	}
    }

    function determinePositionOnWorkplane(event) {
	var planeGeometry = new THREE.PlaneGeometry(1000, 1000);
	var planeMesh = new THREE.Mesh(planeGeometry, new THREE.MeshBasicMaterial({ color: 0x080808, opacity: 0 }));
	planeMesh.doubleSided = true;
	return determinePositionPlane(event, planeMesh);
    }

    function determinePositionPlane(event, planeMesh) {
	
	var mouse = {};
	mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
	mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;
	    
	var vector = new THREE.Vector3( mouse.x, mouse.y, 0.5 );
	var projector = new THREE.Projector();
	var mouse3D = projector.unprojectVector(vector, camera);
	var ray = new THREE.Ray(camera.position, null);
	ray.direction = mouse3D.subSelf(camera.position).normalize();


	var intersects = ray.intersectObject(planeMesh);
	if (intersects.length == 1) {
	    return {x: Math.round(intersects[0].point.x), y: Math.round(intersects[0].point.y)};
	} else {
	    return null;
	}

    }

    function determinePositionOnRay(event, givenRay) {
	var mouse = {};
	mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
	mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;
	    
	var vector = new THREE.Vector3( mouse.x, mouse.y, 0.5 );
	var projector = new THREE.Projector();
	var mouse3D = projector.unprojectVector(vector, camera);
	var mouseRay = new THREE.Ray(camera.position, null);
	mouseRay.direction = mouse3D.subSelf(camera.position).normalize();

	var planeGeometry = new THREE.PlaneGeometry(1000, 1000);
	var planeMesh = new THREE.Mesh(planeGeometry, new THREE.MeshBasicMaterial({ color: 0x080808, opacity: 0 }));

	var intersects = mouseRay.intersectObject(planeMesh);
	if (intersects.length == 1) {
	    mouseRay.origin = intersects[0].point.clone();
	} else {
	    return null;
	}

	// http://softsurfer.com/Archive/algorithm_0106/algorithm_0106.htm
	var u = givenRay.direction.clone().normalize();
	var v = mouseRay.direction.clone().normalize();

	var w0 = new THREE.Vector3().sub(givenRay.origin.clone(), mouseRay.origin.clone());

	var a = u.dot(u), b = u.dot(v), c = v.dot(v), d = u.dot(w0), e = v.dot(w0);
	
	var sc = (b*e - c*d)/(a*c - b*b);
	var tc = (a*e - b*d)/(a*c - b*b);
	
	return  new THREE.Vector3().add(givenRay.origin, u.clone().multiplyScalar(sc));
    }

    function selectObject(event) {
	var mouse = {};
	mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
	mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

	var vector = new THREE.Vector3( mouse.x, mouse.y, 0.5 );
	var projector = new THREE.Projector();
	var mouse3D = projector.unprojectVector(vector, camera);
	var ray = new THREE.Ray(camera.position, null);
	ray.direction = mouse3D.subSelf(camera.position).normalize();
	var intersects = ray.intersectScene(scene);

	var foundObjectId = null;
	if (intersects.length > 0) {
	    for (var i in intersects) {
		if (intersects[i].object.name) {
		    foundObjectId = intersects[i].object.name;
		    break;
		}
	    }
	}

	if (foundObjectId) {
	    if (event.shiftKey) {
		selectionManager.shiftPick(foundObjectId);
	    } else {
		selectionManager.pick(foundObjectId);
	    }
	} else {
	    selectionManager.deselectAll();
	}
    }

    function onMouseUp(event) {

	targetOnDown.azimuth = target.azimuth;
	targetOnDown.elevation = target.elevation;

	if (!panning && !rotating) {

	    var positionOnWorkplane = determinePositionOnWorkplane(event);
	    if (positionOnWorkplane) {
		workplane.clicked(positionOnWorkplane);
	    }

	    if (!SS.constructors.active && (event.button == 0)) {
		selectObject(event);
	    }
	}

	rotating = false;
	panning = false;
	
	popupMenu.onMouseUp(event);

	mouseOnDown = null;
	container.removeEventListener('mouseup', onMouseUp, false);
	container.style.cursor = 'auto';
    }

    function onMouseOut(event) {
	mouseOnDown = null;
	container.removeEventListener('mouseup', onMouseUp, false);
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
    
    function addLights() {
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
	    var color = unselectedColor, opacity = 1.0;
	    var isEditing = geomNode.editing;
	    for (index in geomNode.transforms) {
		if (geomNode.transforms[index].editing) {
		    isEditing = true;
		}
	    }
	    if (isEditing) {
		color = 0x3F8FD2;
		opacity = 0.2;
	    }

	    var material = new THREE.MeshPhongMaterial( { ambient: 0x030303, color: color, opacity: opacity,  specular: 0xccffcc, shininess: 50, shading: THREE.SmoothShading } );
	    var mesh = new THREE.Mesh(geometry, material);
	    mesh.doubleSided = true;
	    mesh.name = geomNode.id;
	    scene.addObject(mesh);
	    idToModel[geomNode.id] = mesh;
        }
    }

    var remove = function(geomNode) {
	var mesh = idToModel[geomNode.id];
	if (mesh) {
	    scene.removeObject(mesh);
	    delete idToModel[geomNode.id];
	}
    }

    this.selectionUpdated = function(event) {
        if (event.deselected) {
            for (var i in event.deselected) {
                var id = event.deselected[i];
		idToModel[id].materials[0].color.setHex(unselectedColor);
            }
        }
        if (event.selected) {
            for (var i in event.selected) {
                var id = event.selected[i];
		idToModel[id].materials[0].color.setHex(selectedColor);
		
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
    this.workplane = workplane;
    this.determinePositionOnRay = determinePositionOnRay;
    this.determinePositionPlane = determinePositionPlane;
    this.popupMenu = popupMenu;
    this.onMouseUp = onMouseUp;
    this.createGeometry = createGeometry;

    return this;
}
