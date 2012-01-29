var SS = SS || {};
SS.SceneView = function(container) {

    var camera, scene, renderer, w, h;
    var overRenderer;

    var mouseOnDown, lastMouseMpos;

    var elevation = 0, azimuth = Math.PI/4;
    var target = { azimuth: Math.PI/4, elevation: Math.PI*3/8 };
    var targetOnDown = { azimuth: target.azimuth, elevation: target.elevation };
    var distance = 1000, distanceTarget = 300;
    var targetScenePosition = new THREE.Vector3(0,0,0);

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
        lastMousePos = mouseOnDown;

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

        var mouse = {};
	mouse.x = event.clientX;
	mouse.y = event.clientY;

	if (mouseOnDown) {
	    
	    if (!state && !popupMenu.isShowing()) {
                var overThreshold = ((Math.abs(event.clientX - mouseOnDown.x) > threshhold)
			             ||
			             (Math.abs(event.clientY - mouseOnDown.y) > threshhold));
		if ((event.button === 0 && !event.shiftKey) && overThreshold) {
                    state = 'rotating';
		} 
		if ((event.button === 1 || event.shiftKey) && overThreshold)  {
                    state = 'panning';
		}
	    }
	    
	    if (state) {
		popupMenu.cancel()
	    } 
            
            if (state === 'rotating') {
		
		var zoomDamp = Math.sqrt(distance)/10;

		target.azimuth = targetOnDown.azimuth - (mouse.x - mouseOnDown.x) * 0.005 * zoomDamp;
		target.elevation = targetOnDown.elevation - (mouse.y - mouseOnDown.y) * 0.005 * zoomDamp;

		target.elevation = target.elevation > Math.PI ? Math.PI : target.elevation;
		target.elevation = target.elevation < 0 ? 0 : target.elevation;
	    } else if (state === 'panning') {
                
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

        lastMousePos = mouse;
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

        scene.position.addSelf(new THREE.Vector3().sub(targetScenePosition, scene.position).multiplyScalar(0.2));

	camera.position.x = distance * Math.sin(elevation) * Math.cos(azimuth);
	camera.position.y = distance * Math.sin(elevation) * Math.sin(azimuth);
	camera.position.z = distance * Math.cos(elevation);
        camera.position.addSelf(scene.position);

        camera.lookAt(scene.position);
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

	    var triangleGeometries = create3DGeometries(geomNode.mesh['faces']);
	    var triangleMaterial = new THREE.MeshPhongMaterial( { ambient: color, color: color, opacity: opacity,  specular: color, shininess: 50, shading: THREE.SmoothShading } );
            var triangles = new THREE.Object3D();
            triangleGeometries.map(function(triangleGeometry) {
                var mesh = new THREE.Mesh(triangleGeometry, triangleMaterial);
                mesh.doubleSided = true;
                mesh.name = objectName
                triangles.add(mesh);
            });
            triangles.name = objectName;

            var lineGeometries = create1DGeometries(geomNode.mesh['edges']);
            var lineMaterial = new THREE.LineBasicMaterial({ color: color, opacity: 1.0, linewidth: 2 });
            var lines = new THREE.Object3D();
            lineGeometries.map(function(lineGeometry) {
                var line = new THREE.Line(lineGeometry, lineMaterial);
                line.name = objectName;
                lines.add(line);
            });
            lines.name = objectName;

            var selectionGeometriesForLines = create1DSelectionGeometries(geomNode.mesh['edges']);
            var selectionMeshes = new THREE.Object3D();
            selectionGeometriesForLines.map(function(selectionGeometriesForLine) {
                var mesh = new THREE.Mesh(selectionGeometriesForLine, new THREE.MeshBasicMaterial({ color: 0x666666, opacity: 0 })); 
                mesh.name = objectName
                mesh.doubleSided = true;
                selectionMeshes.add(mesh);
            });
            selectionMeshes.name = objectName;                       
            
	    scene.add(triangles);
	    scene.add(lines);
	    scene.add(selectionMeshes);
	    idToModel[geomNode.id] = {'faces': triangles, 
                                      'edges': lines,
                                      'selectionForEdges' : selectionMeshes};
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
                
		idToModel[id]['faces'].children.map(function(child) {
                    child.material.color.setHex(unselectedColor);
		    child.material.ambient.setHex(unselectedColor);
		    child.material.specular.setHex(unselectedColor);
                });
		idToModel[id]['edges'].children.map(function(child) {
                    child.material.color.setHex(unselectedColor);
                });
            }
        }
        if (event.selected) {
            for (var i in event.selected) {
                var id = event.selected[i];

	        idToModel[id]['faces'].children.map(function(child) {
                    child.material.color.setHex(selectedColor);
		    child.material.ambient.setHex(selectedColor);
		    child.material.specular.setHex(selectedColor);
                });
		idToModel[id]['edges'].children.map(function(child) {
                    child.material.color.setHex(selectedColor);
                });
		
            }
        }
    }

   var create1DGeometries = function(mesh) {

       var precisionPoints = 4; 
       var precision = Math.pow( 10, precisionPoints );
       var keyForVertex = function(vertex) {
           var position = vertex.position;
           return [ Math.round( position.x * precision ), Math.round( position.y * precision ), Math.round( position.z * precision ) ].join( '_' );
       }

       var segmentsByStartPosition = {};
       var segmentsByEndPosition = {};
       var allSegments = [];

       mesh.segments.map(function(segment) {

           var vertices = [];
           for (var i = segment.start; i < segment.end; i += 3) {
               var position = new THREE.Vector3(mesh.positions[i], 
					        mesh.positions[i + 1], 
					        mesh.positions[i + 2]);
	       var vertex = new THREE.Vertex(position);
               vertices.push(vertex);
           }
           
           var startVertex = vertices[0];
           var endVertex = vertices[vertices.length-1];
           var startVertexKey = keyForVertex(startVertex);
           var endVertexKey = keyForVertex(endVertex);
           
           allSegments.push(vertices);
           segmentsByStartPosition[startVertexKey] = segmentsByStartPosition[startVertexKey] || [];
           segmentsByStartPosition[startVertexKey].push(vertices);

           segmentsByEndPosition[endVertexKey] = segmentsByEndPosition[endVertexKey] || [];
           segmentsByEndPosition[endVertexKey].push(vertices);
       });

       // Compression
       do {

           var didACompression = false;
           for (vertexKey in segmentsByStartPosition)  {

               if (!didACompression) {
                   // If the is a segment with the same end position key as this 
                   // start position, compress the segments

                   if ((segmentsByStartPosition[vertexKey].length > 0) &&
                       segmentsByEndPosition[vertexKey] &&
                       (segmentsByEndPosition[vertexKey].length > 0)) {

                       var firstSegment = segmentsByEndPosition[vertexKey][0];
                       var secondSegment = segmentsByStartPosition[vertexKey][0];

                       // Circular segments
                       if (!(firstSegment === secondSegment)) {

                           var newSegment = [].concat(firstSegment).concat(secondSegment);

                           newSegmentStartKey = keyForVertex(newSegment[0]);
                           newSegmentEndKey   = keyForVertex(newSegment[newSegment.length - 1]);
                           
                           allSegments.splice(allSegments.indexOf(firstSegment), 1);
                           allSegments.splice(allSegments.indexOf(secondSegment), 1);
                           allSegments.push(newSegment);

                           segmentsByStartPosition[vertexKey].splice(
                               segmentsByStartPosition[vertexKey].indexOf(secondSegment), 1);
                           segmentsByStartPosition[newSegmentStartKey].splice(
                               segmentsByStartPosition[newSegmentStartKey].indexOf(firstSegment), 1);
                           
                           segmentsByEndPosition[vertexKey].splice(
                               segmentsByEndPosition[vertexKey].indexOf(firstSegment), 1);
                           segmentsByEndPosition[newSegmentEndKey].splice(
                               segmentsByEndPosition[newSegmentEndKey].indexOf(secondSegment), 1);
                           

                           segmentsByStartPosition[newSegmentStartKey].push(newSegment);
                           segmentsByEndPosition[newSegmentEndKey].push(newSegment);

                           didACompression = true;
                       }
                   }
               }
               
           }
               
           
       } while(didACompression);
       

       return allSegments.map(function(segment) {
           var geometry =  new THREE.Geometry();
           geometry.vertices = segment;
           return geometry;
       });

    }

   var create1DSelectionGeometries = function(mesh) {
       return mesh.segments.map(function(segment) {
           var positions = [];
           for (var i = segment.start; i < segment.end; i+=3) {
            positions.push(new THREE.Vector3(mesh.positions[i], 
					     mesh.positions[i + 1], 
					     mesh.positions[i + 2]));
           }
           return new THREE.PipeGeometry(3, positions);
       });
    }

    var createGeometry = function(meshes) {
        return {'faces' : create3DGeometries(meshes['faces']),
                'edges' : create1DGeometries(meshes['edges'])};
    }

    var create3DGeometries = function(mesh) {
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
	return [geometry];
    }

    this.setOthersTransparent = function(geomNode) {
        geom_doc.rootNodes.map(function(rootNode) {
            if (geomNode.id !== rootNode.id) {
                for (key in idToModel[rootNode.id]) {
                    idToModel[rootNode.id][key].children.map(function(child) {
                        if (child.material.opacity === 1.0) {
                            child.material.opacity = 0.2;
                        }
                    });
                }
            }
         });
    }

    this.restoreOpacity = function() {
        geom_doc.rootNodes.map(function(rootNode) {
            if (idToModel[rootNode.id]) {
                for (key in idToModel[rootNode.id]) {
                    idToModel[rootNode.id][key].children.map(function(child) {
                        if (child.material.opacity === 0.2) {
                            child.material.opacity = 1.0;
                        }
                    });
                }
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
