var SS = SS || {};
SS.SceneView = function(container) {

    var camera, scene, renderer, w, h;
    var overRenderer;
    var lastCameraPosition = new THREE.Vector3(0,0,0);

    var mouseOnDown, lastMouseMpos, mouseDownOnActiveSceneObject;

    var elevation = Math.PI*2/8, azimuth = -4*Math.PI/9;
    var target = { azimuth: -4*Math.PI/9, elevation: Math.PI*2/8 };
    var targetOnDown = { azimuth: target.azimuth, elevation: target.elevation };
    var distance = 1000, distanceTarget = 300;
    var targetScenePosition = new THREE.Vector3(0,0,0);

    var workplane;
    var popupMenu = SS.popupMenu();

    var that = this;
    
    _.extend(that, Backbone.Events);
    
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
	renderer.autoClear = true;
	renderer.setClearColorHex(0x080808, 0.0);
	renderer.setSize(w, h);
        renderer.sortObjects = false;

	container.appendChild( renderer.domElement );
        
	container.style.color = '#fff';
	container.style.font = '13px/20px Arial, sans-serif';
        
        scene.add( new THREE.AmbientLight( 0x404040 ) );
        
        addLights();
	workplane = new SS.Workplane({scene: scene});
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

        mouseDownButton = event.button;
	mouseOnDown = {};
	mouseOnDown.x = event.clientX;
	mouseOnDown.y = event.clientY;
        lastMousePos = mouseOnDown;

	document.addEventListener('mouseup', onMouseUp, false);

        SS.UI_MOUSE_STATE.free();

        var mouseOverActiveObjects = mouseOverSceneObjectViews.filter(function(object) {
            return object.active;
        });
        mouseDownOnActiveSceneObject = mouseOverActiveObjects.length > 0;
        
	that.triggerMouseDownOnSceneObjectViews(event);
        that.updateScene = true;
    }
    
    function onMouseMove(event) {

	that.triggerMouseOverSceneObjectViews(event);

        var mouse = {};
	mouse.x = event.clientX;
	mouse.y = event.clientY;

	if (mouseOnDown) {

            var panRotateThreshold = 10;
            var transformerThreshold = 5;
	    
	    if (SS.UI_MOUSE_STATE.isFree()) {
                
                var overPanRotateThreshold = ((Math.abs(event.clientX - mouseOnDown.x) > panRotateThreshold)
			                      ||
			                      (Math.abs(event.clientY - mouseOnDown.y) > panRotateThreshold));
                if (overPanRotateThreshold) {

		    if (!event.shiftKey &&
                        (event.button === 0) &&
                        (mouseDownButton === 0) &&
                        !mouseDownOnActiveSceneObject) {
                        
                        SS.UI_MOUSE_STATE.rotating = true;
		    } 
		    if (((event.button === 1) && (mouseDownButton === 1)) 
                        || 
                        ((event.button === 0) && (mouseDownButton === 0) && (event.shiftKey)))  {
                        SS.UI_MOUSE_STATE.panning = true;
		    }
                }
	    }

            if (SS.UI_MOUSE_STATE.rotating) {
		
		var zoomDamp = Math.sqrt(distance)/10;

		target.azimuth = targetOnDown.azimuth - (mouse.x - mouseOnDown.x) * 0.005 * zoomDamp;
		target.elevation = targetOnDown.elevation - (mouse.y - mouseOnDown.y) * 0.005 * zoomDamp;

		target.elevation = target.elevation > Math.PI ? Math.PI : target.elevation;
		target.elevation = target.elevation < 0 ? 0 : target.elevation;

	    } else if (SS.UI_MOUSE_STATE.panning) {
                
                var dMouse = {x: mouse.x - lastMousePos.x,
                              y: mouse.y - lastMousePos.y};
                
                var camVec = camera.position.clone().negate().normalize();
                var upVec = new THREE.Vector3(0,0,1);
                var mouseLeftVec = new THREE.Vector3().cross(upVec, camVec);
                var mouseUpVec = new THREE.Vector3().cross(camVec, mouseLeftVec);
                
                var dPos = mouseLeftVec.clone().multiplyScalar(dMouse.x).addSelf(mouseUpVec.clone().multiplyScalar(dMouse.y));
                var factor = Math.sqrt(distance)/50;
                dPos.multiplyScalar(factor);

                targetScenePosition.addSelf(dPos);
            }
            
	} 


        var mouseOverActiveObjects = mouseOverSceneObjectViews.filter(function(object) {
            return object.active;
        });
        if (mouseOverActiveObjects.length > 0) {
            document.body.style.cursor = 'pointer';
        } else if (SS.transformerManager && SS.transformerManager.cursor) {
            document.body.style.cursor = SS.transformerManager.cursor;
        } else {
            document.body.style.cursor = 'default';
        }

	var positionOnWorkplane = determinePositionOnWorkplane(event);
	workplane.updateXYLocation(positionOnWorkplane, event);

	var origin = new THREE.Vector3(0, 0, 0);
	var direction = new THREE.Vector3(0, 0, 1);
	var ray = new THREE.Ray(origin, direction);
	var positionOnVertical = that.determinePositionOnRay(event, ray);
        if (positionOnVertical) {
            workplane.updateZLocation(positionOnVertical, event);
        }

        lastMousePos = mouse;
        that.updateScene = true;
    }

    function determinePositionOnWorkplane(event) {
	var planeGeometry = new THREE.PlaneGeometry(1000, 1000);
	var planeMesh = new THREE.Mesh(planeGeometry, new THREE.MeshBasicMaterial({ color: 0x080808, opacity: 0 }));
	planeMesh.doubleSided = true;
	return determinePositionOnPlane(event, planeMesh);
    }

    function determinePositionOnPlane(event, planeMesh) {
	
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
	    return {x: Math.round(intersects[0].point.x), 
                    y: Math.round(intersects[0].point.y),
                    z: Math.round(intersects[0].point.z)};
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
	
        var found = SS.selectGeomNodesInScene(scene, camera, event);
        var foundGeomNodes = found.filter(function(obj) {
            return obj.object.name.geomNodeId;
        });

	if (foundGeomNodes.length > 0) {
	    if (event.ctrlKey || event.metaKey) {
		selectionManager.shiftPick(foundGeomNodes[0].object.name.geomNodeId);
	    } else if (!event.shiftKey) {
		selectionManager.pick(foundGeomNodes[0].object.name.geomNodeId);
	    }
	} else {
	    selectionManager.deselectAll();
	}
    }

    function onMouseUp(event) {

        that.triggerMouseUpOnSceneObjectViews(event);

	targetOnDown.azimuth = target.azimuth;
	targetOnDown.elevation = target.elevation;

	if (SS.UI_MOUSE_STATE.isFree() && 
            !SS.UI_EDITING_STATE.isEditing() && 
            (event.button == 0)) {
	    selectObject(event);
	}

	popupMenu.onMouseUp(event);

	mouseOnDown = null;
	container.removeEventListener('mouseup', onMouseUp, false);
        that.updateScene = true;
        
    }

    function onMouseOut(event) {
	mouseOnDown = null;
	container.removeEventListener('mouseup', onMouseUp, false);
    }

    function onMouseWheel(event) {
        var factor = 0.01;
	event.preventDefault();
	if (overRenderer) {
	    if (event.wheelDeltaY) {
		zoom(event.wheelDeltaY * Math.sqrt(distance)*factor);
	    }
	    if (event.detail) {
		zoom(-event.detail*60 * Math.sqrt(distance)*factor);
	    }
	}
	return false;
    }

    function onDocumentKeyDown(event) {
        var factor = 10;
	if (overRenderer) {
	    switch (event.keyCode) {
	    case 187:
	    case 107:
		zoom(Math.sqrt(distance)*factor);
		event.preventDefault();
		break;
	    case 189:
	    case 109:
		zoom(-Math.sqrt(distance)*factor);
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
        that.trigger('cameraChange');
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

        var dScenePosition = new THREE.Vector3().sub(targetScenePosition, scene.position).multiplyScalar(0.2);
        scene.position.addSelf(dScenePosition);

        lastCameraPosition = camera.position.clone();

	camera.position.x = distance * Math.sin(elevation) * Math.cos(azimuth);
	camera.position.y = distance * Math.sin(elevation) * Math.sin(azimuth);
	camera.position.z = distance * Math.cos(elevation);
        camera.position.addSelf(scene.position);

        var dCameraPosition = new THREE.Vector3().sub(camera.position, lastCameraPosition);
        if ((dScenePosition.length() > 0.1) || (dCameraPosition.length() > 0.1)) {
            that.trigger('cameraChange');
            camera.lookAt(scene.position);
            that.updateScene = true;
        }

        if (that.updateScene) {
	    renderer.render(scene, camera);
            that.updateScene = false;
        }

    }
    
    function addLights() {

	pointLight = new THREE.PointLight( 0x999999 );
	pointLight.position.set(0, 0, 100);
        scene.add(pointLight);

        pointLight = new THREE.PointLight( 0x999999 );
	pointLight.position.set(0, 0, -100);
        scene.add(pointLight);
    }

    var sceneObjectViews = [];
    var mouseOverSceneObjectViews = [];
    var mouseDownSceneObjectViews = [];
    var sortByPriority = function(a,b) {
        return a.priority < b.priority;
    };

    this.addToMouseOverAndMouseDown = function(view) {
        [mouseOverSceneObjectViews, mouseDownSceneObjectViews].map(function(array) {
            array.push(view);
            array = array.sort(sortByPriority);
        });
    }

    this.registerSceneObjectView = function(sceneObjectView) {
	sceneObjectViews.push(sceneObjectView);
    }

    this.deregisterSceneObjectView = function(sceneObjectView) {
        [sceneObjectViews, mouseOverSceneObjectViews, mouseDownSceneObjectViews].map(function(array) {
            var index = array.indexOf(sceneObjectView);
            if (index !== -1) {
	        array.splice(index, 1);
            }
        });
    }

    var findSceneObjectViewsForEvent = function(event) {
	
	var found = SS.selectNonGeomNodesInScene(scene, camera, event);
	var objects = _.pluck(found, 'object');

	// Select the hightest-level THREE.Object3D objects in the scene
	var getRoot = function(object) {
	    return object.parent.constructor === THREE.Scene ? 
		object : 
		getRoot(object.parent);
	}

	var foundSceneObjectViews = [];
	objects.map(getRoot).map(function(object) {
	    sceneObjectViews.map(function(sceneObjectView) {
		if (getRoot(object) === sceneObjectView.sceneObject) {
                    if (foundSceneObjectViews.indexOf(sceneObjectView) === -1) {
		        foundSceneObjectViews.push(sceneObjectView);
                    }
		}
	    });
	});
        
        return foundSceneObjectViews.sort(sortByPriority);
 
    }

    if (SS.UI_MOUSE_STATE.isFree()) {
        this.triggerMouseOverSceneObjectViews = function(event) {
	    var previousOverObjects = mouseOverSceneObjectViews.map(function(x) { return x; });
            var leaveObjects = mouseOverSceneObjectViews.map(function(x) { return x; });
            mouseOverSceneObjectViews = [];
	    findSceneObjectViewsForEvent(event).map(function(sceneObjectView) {
                if (previousOverObjects.indexOf(sceneObjectView) === -1) {
		    sceneObjectView.trigger('mouseEnter', event);
	        }
	        mouseOverSceneObjectViews.push(sceneObjectView);
                var index = leaveObjects.indexOf(sceneObjectView);
                if (index !== -1) {
	            leaveObjects.splice(index, 1);
                }
	    });
	    leaveObjects.map(function(sceneObjectView) {
	        sceneObjectView.trigger('mouseLeave', event);
	    });

            if ((event.button === 0) &&
                (event.ctrlKey || !event.shiftKey)) {

                if (mouseDownSceneObjectViews.length > 0) {
                    mouseDownSceneObjectViews[0].trigger('mouseDrag', event);
                }
            }

        }
    }

    this.triggerMouseDownOnSceneObjectViews = function(event) {
        mouseDownSceneObjectViews = findSceneObjectViewsForEvent(event);

        if ((event.button === 0) &&
            (mouseDownSceneObjectViews.length > 0)) {
            mouseDownSceneObjectViews[0].trigger('mouseDown', event);
        }
    }

    this.triggerMouseUpOnSceneObjectViews = function(event) {
        var sceneObjects = findSceneObjectViewsForEvent(event);
        if (sceneObjects.length > 0) {
            sceneObjects[0].trigger('mouseUp', event);
        }
        mouseDownSceneObjectViews = [];
    }

    init();
    this.animate = animate;
    this.renderer = renderer;
    this.scene = scene;
    this.camera = camera;
    this.workplane = workplane;
    this.determinePositionOnRay = determinePositionOnRay;
    this.determinePositionOnPlane = determinePositionOnPlane;
    this.determinePositionOnWorkplane = determinePositionOnWorkplane;
    this.popupMenu = popupMenu;
    this.onMouseUp = onMouseUp;
    this.onMouseMove = onMouseMove;
    this.onMouseDown = onMouseDown;

   
    return this;
}
