var SS = SS || {};
SS.SceneView = function(container) {

    var camera, scene, renderer, w, h;
    var overRenderer;

    var mouseOnDown = null;
    var lastMouseDownTime = null;

    var elevation = 0, azimuth = Math.PI/4;
    var target = { azimuth: Math.PI/4, elevation: Math.PI*3/8 };
    var targetOnDown = { azimuth: target.azimuth, elevation: target.elevation };
    var distance = 1000, distanceTarget = 300;

    var idToModel = {};
    var unselectedColor = 0x00dd00, selectedColor = 0xdddd00;
    var state, threshhold = 10;

    var workplane, cursoid;
    var popupMenu = SS.popupMenu();
    
    function init() {

	w = container.offsetWidth || window.innerWidth;
	h = container.offsetHeight || window.innerHeight;

        scene = new THREE.Scene();

        camera = new THREE.PerspectiveCamera(30, w / h, 1, 10000);
	camera.up.x = 0;
	camera.up.y = 0;
	camera.up.z = 1;
        scene.add( camera );

	renderer = new THREE.WebGLRenderer({antialias: true});
	renderer.autoClear = false;
	renderer.setClearColorHex(0x080808, 0.0);
	renderer.setSize(w, h);

	container.appendChild( renderer.domElement );
        
	container.style.color = '#fff';
	container.style.font = '13px/20px Arial, sans-serif';
        
        scene.add( new THREE.AmbientLight( 0x404040 ) );
        
        addLights();
	workplane = new SS.Workplane({scene: scene});
        cursoid = new SS.Cursoid({scene: scene, workplane: workplane});
	popupMenu = SS.popupMenu();

	renderer.domElement.style.position = 'absolute';

        container.addEventListener('mousedown', onMouseDown, false);
	container.addEventListener('mousewheel', onMouseWheel, false);
	container.addEventListener('DOMMouseScroll', onMouseWheel, false);
	container.addEventListener('mousemove', onMouseMove, false);
	window.addEventListener('keydown', onDocumentKeyDown, false);
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

        state = undefined;
	container.addEventListener('mouseup', onMouseUp, false);

	var cursoidName;
	var activeConstructor = SS.constructors.active;
	if (activeConstructor) {
	    var anchorName = activeConstructor.getAnchor(scene, camera, event);
	    if (anchorName) {
		var initialCursoid = activeConstructor.activateAnchor(anchorName);
		cursoidName = initialCursoid;
	    }
	}
	
	if (!cursoidName) {
	    cursoidName = cursoid.getCursoid(scene, camera, event)
	}
        if (cursoidName) {
            state = {cursoid: cursoidName};
            cursoid.activate(cursoidName);
	    popupMenu.cancel();
        } 
    }
    
    function onMouseMove(event) {

	if (mouseOnDown) {
	    
	    if (!state && !popupMenu.isShowing()) {
		if (event.button === 0 && !event.shiftKey) {
		    if ((Math.abs(event.clientX - mouseOnDown.x) > threshhold)
			||
			(Math.abs(event.clientY - mouseOnDown.y) > threshhold)) {
			
                        state = 'rotating';
		    }
		} 
		if (event.button === 1 || event.shiftKey) {
		    if ((Math.abs(event.clientX - mouseOnDown.x) > threshhold)
			||
			(Math.abs(event.clientY - mouseOnDown.y) > threshhold)) {
                        
                        state = 'panning';
		    }
		}
	    }
	    
	    if (state) {
		popupMenu.cancel()
	    } 
            
            if (state === 'rotating') {
		var mouse = {};
		mouse.x = event.clientX;
		mouse.y = event.clientY;
		var zoomDamp = Math.sqrt(distance)/10;

		target.azimuth = targetOnDown.azimuth - (mouse.x - mouseOnDown.x) * 0.005 * zoomDamp;
		target.elevation = targetOnDown.elevation - (mouse.y - mouseOnDown.y) * 0.005 * zoomDamp;

		target.elevation = target.elevation > Math.PI ? Math.PI : target.elevation;
		target.elevation = target.elevation < 0 ? 0 : target.elevation;
	    }
	} 


        if (cursoid.getCursoid(scene, camera, event) ||
	    (SS.constructors.active &&  
	     SS.constructors.active.getAnchor(scene, camera, event))) {
            document.body.style.cursor = 'pointer';
        } else {
            document.body.style.cursor = 'default';
        }

	var positionOnWorkplane = determinePositionOnWorkplane(event);
	workplane.updateXYLocation(positionOnWorkplane, event);

	var origin = new THREE.Vector3(0, 0, 0);
	var direction = new THREE.Vector3(0, 0, 1);
	var ray = new THREE.Ray(origin, direction);
	var positionOnVertical = sceneView.determinePositionOnRay(event, ray);
        if (positionOnVertical) {
            workplane.updateZLocation(positionOnVertical, event);
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
	
        var found = SS.selectInScene(scene, camera, event);
        var foundGeomNodes = found.filter(function(obj) {
            return obj.object.name.geomNodeId;
        });

	if (foundGeomNodes.length > 0) {
	    if (event.shiftKey) {
		selectionManager.shiftPick(foundGeomNodes[0].object.name.geomNodeId);
	    } else {
		selectionManager.pick(foundGeomNodes[0].object.name.geomNodeId);
	    }
	} else {
	    selectionManager.deselectAll();
	}
    }

    function onMouseUp(event) {

	targetOnDown.azimuth = target.azimuth;
	targetOnDown.elevation = target.elevation;

	if (!state) {

	    var positionOnWorkplane = determinePositionOnWorkplane(event);
	    if (positionOnWorkplane) {
		workplane.clicked(positionOnWorkplane);
	    }

	    if (!SS.constructors.active && (event.button == 0)) {
		selectObject(event);
	    }
	}

	state = undefined;
        cursoid.deactivate();
	//SS.constructors.active && SS.constructors.active.deactivateAnchor();
	popupMenu.onMouseUp(event);

	mouseOnDown = null;
	container.removeEventListener('mouseup', onMouseUp, false);
    }

    function onMouseOut(event) {
	mouseOnDown = null;
	container.removeEventListener('mouseup', onMouseUp, false);
    }

    function onMouseWheel(event) {
	event.preventDefault();
	if (overRenderer) {
	    if (event.wheelDeltaY) {
		zoom(event.wheelDeltaY * 0.05);
	    }
	    if (event.detail) {
		zoom(-event.detail*60 * 0.05);
	    }
	}
	return false;
    }

    function onDocumentKeyDown(event) {
	if (overRenderer) {
	    switch (event.keyCode) {
	    case 187:
	    case 107:
		zoom(100);
		event.preventDefault();
		break;
	    case 189:
	    case 109:
		zoom(-100);
		event.preventDefault();
		break;
	    }
	}
	return false;
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

        camera.lookAt( scene.position );
	renderer.render(scene, camera);
    }
    
    function addLights() {

	pointLight = new THREE.PointLight( 0x999999 );
	pointLight.position.set(0, 0, 100);
        scene.add(pointLight);

        pointLight = new THREE.PointLight( 0x999999 );
	pointLight.position.set(0, 0, -100);
        scene.add(pointLight);


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

	    var color = unselectedColor, opacity = 1.0;
	    var isEditing = geomNode.editing;
	    for (index in geomNode.transforms) {
		if (geomNode.transforms[index].editing) {
		    isEditing = true;
		}
	    }
	    if (isEditing) {
		color = 0xdddd00;
		opacity = 0.5;
	    }

            var objectName = {geomNodeId: geomNode.id};

	    var geometry3D = createGeometry(geomNode.mesh);
	    var material3D = new THREE.MeshPhongMaterial( { ambient: color, color: color, opacity: opacity,  specular: color, shininess: 50, shading: THREE.SmoothShading } );
            var mesh3d = new THREE.Mesh(geometry3D, material3D);
            mesh3d.doubleSided = true;
            mesh3d.name = objectName
            
            var geometry1D = create1DGeometry(geomNode.mesh['1d']);
            var material1D = new THREE.LineBasicMaterial({ color: color, opacity: 1.0, linewidth: 2 });
            var mesh1d = new THREE.Line(geometry1D, material1D);
            mesh1d.name = objectName

            var selectionGeometry1D = create1DSelectionGeometry(geomNode.mesh['1d']);
            var selectionMesh1d = new THREE.Mesh(selectionGeometry1D, new THREE.MeshBasicMaterial({ color: 0x666666, opacity: 0.0 })); 
            selectionMesh1d.name = objectName
            selectionMesh1d.doubleSided = true;
            
	    scene.add(mesh3d);
	    scene.add(mesh1d);
	    scene.add(selectionMesh1d);
	    idToModel[geomNode.id] = {'1d': mesh1d, '3d': mesh3d, 'selection1d' : selectionMesh1d};
        }
    }

    var remove = function(geomNode) {
	var meshes = idToModel[geomNode.id];
	if (meshes) {
            for (key in meshes) {
	        scene.remove(meshes[key]);
            }
	    delete idToModel[geomNode.id];
	}
    }

    this.selectionUpdated = function(event) {
        if (event.deselected) {
            for (var i in event.deselected) {
                var id = event.deselected[i];
                
		idToModel[id]['3d'].material.color.setHex(unselectedColor);
		idToModel[id]['3d'].material.ambient.setHex(unselectedColor);
		idToModel[id]['3d'].material.specular.setHex(unselectedColor);
		idToModel[id]['1d'].material.color.setHex(unselectedColor);
            }
        }
        if (event.selected) {
            for (var i in event.selected) {
                var id = event.selected[i];

	        idToModel[id]['3d'].material.color.setHex(selectedColor);
		idToModel[id]['3d'].material.ambient.setHex(selectedColor);
		idToModel[id]['3d'].material.specular.setHex(selectedColor);
		idToModel[id]['1d'].material.color.setHex(selectedColor);
		
            }
        }
    }

   var create1DGeometry = function(mesh) {
        var geometry = new THREE.Geometry();

        for (var i = 0; i < mesh.positions.length/3; ++i) {
            var position = new THREE.Vector3(mesh.positions[i * 3], 
					     mesh.positions[i * 3 + 1], 
					     mesh.positions[i * 3 + 2]);
	    var vertex = new THREE.Vertex(position);
            geometry.vertices.push(vertex);
        }
        return geometry;
    }

   var create1DSelectionGeometry = function(mesh) {
        var positions = [];
        for (var i = 0; i < mesh.positions.length/3; ++i) {
            positions.push(new THREE.Vector3(mesh.positions[i * 3], 
					     mesh.positions[i * 3 + 1], 
					     mesh.positions[i * 3 + 2]));
        }
       return new THREE.PipeGeometry(3, positions);
    }

    var createGeometry = function(meshes) {
        return create3DGeometry(meshes['3d']);
    }

    var create3DGeometry = function(mesh) {
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

    this.setOthersTransparent = function(geomNode) {
        geom_doc.rootNodes.map(function(rootNode) {
            if (geomNode.id !== rootNode.id) {
                idToModel[rootNode.id]['3d'].material.opacity = 0.2;
            }
         });
    }

    this.restoreOpacity = function() {
        geom_doc.rootNodes.map(function(rootNode) {
            if (idToModel[rootNode.id]) {
                idToModel[rootNode.id]['3d'].material.opacity = 1.0;
            }
        });
    }
    
    init();
    this.animate = animate;
    this.renderer = renderer;
    this.scene = scene;
    this.workplane = workplane;
    this.cursoid = cursoid;
    this.determinePositionOnRay = determinePositionOnRay;
    this.determinePositionPlane = determinePositionPlane;
    this.popupMenu = popupMenu;
    this.onMouseUp = onMouseUp;
    this.createGeometry = createGeometry;

    return this;
}
