var SS = SS || {};
SS.constructors = {};
SS.preview = {};
SS.modifier = {};

SS.GeomNodeUpdater = function(geomNode) {
    var geomNode = geomNode, that = this;

    evented(this);

    var updateTreeview = function() {
	$('#x').val(geomNode.origin.x);
	$('#y').val(geomNode.origin.y);
	$('#z').val(geomNode.origin.z);
	$('#r1').val(geomNode.parameters.r1);
	$('#r2').val(geomNode.parameters.r2);
    }

    this.setOrigin = function(origin) {
        geomNode.origin = origin;
        that.fire({type: 'updated'});
	updateTreeview();
    }

    this.setR1R2 = function(r1r2) {
	geomNode.parameters.r1 = r1r2.r1;
	geomNode.parameters.r2 = r1r2.r2;
        that.fire({type: 'updated'});
	updateTreeview();
    }

    updateTreeview();
}

SS.modifier.Origin = function(spec) {
    var geomNode = spec.geomNode, updater = spec.updater, cursoid = spec.cursoid;
    
    var setOrigin = function(event) {
	updater.setOrigin({x: event.position.x,
                           y: event.position.y,
                           z: event.position.z});
    }
    
    cursoid.on('cursoidUpdated', setOrigin);

    this.dispose = function() {
	cursoid.off('cursoidUpdated', setOrigin);
    }
}

SS.modifier.MajorMinorRadii = function(spec) {
    var geomNode = spec.geomNode, updater = spec.updater, cursoid = spec.cursoid;
    
    var setR1R2 = function(event) {
	var r1 = Math.abs(event.position.x - geomNode.origin.x);
	var r2 = Math.abs(event.position.y - geomNode.origin.y);
	updater.setR1R2({r1: r1, r2:r2});
    }
    
    cursoid.on('cursoidUpdated', setR1R2);

    this.dispose = function() {
	cursoid.off('cursoidUpdated', setR1R2);
    }
}


SS.preview.dimTextMaterial 
    = new THREE.MeshBasicMaterial({ color: 0x66A1D2, opacity: 0.8, wireframe: false } );
SS.preview.dimLineMaterial 
    = new THREE.LineBasicMaterial({ color: 0x66A1D2, opacity: 0.5, wireframe : true });
SS.preview.arrowMaterial 
    = new THREE.MeshBasicMaterial({ color: 0x66A1D2, opacity: 0.5 });

SS.preview.createDimArrow = function(value, angle) {
    var r1DimensionGeom = new THREE.Geometry();
    r1DimensionGeom.vertices.push(
	new THREE.Vertex(new THREE.Vector3(0,0,0)));
    r1DimensionGeom.vertices.push(
	new THREE.Vertex(new THREE.Vector3(value, 0, 0)));
    var line = new THREE.Line(r1DimensionGeom, SS.constructors.lineMaterial);
    
    var arrowGeometry = new THREE.Geometry();
    arrowGeometry.vertices.push(
	new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
    arrowGeometry.vertices.push(
	new THREE.Vertex(new THREE.Vector3(-3, 1, 0)));
    arrowGeometry.vertices.push(
	new THREE.Vertex(new THREE.Vector3(-3, -1, 0)));
    
    var arrowFace = new THREE.Face3(0,1,2);
    arrowGeometry.faces.push(arrowFace);
    arrowGeometry.computeCentroids();
    arrowGeometry.computeFaceNormals();
    
    var arrowFace = new THREE.Face3(0,1,2);
    arrowGeometry.faces.push(arrowFace);
    arrowGeometry.computeCentroids();
    arrowGeometry.computeFaceNormals();
    var arrow =  new THREE.Mesh(arrowGeometry, SS.preview.arrowMaterial);
    arrow.position.x = value;
    arrow.doubleSided = true;
    
    var text = SS.preview.createDimText(value);
    if ((angle > Math.PI/4) && (angle < Math.PI*5/4)) {
	text.position.x = value/2;
	text.position.y = 1;

    } else {
	text.position.x = value/2;
	text.position.y = -1;
	text.rotation.z = Math.PI;
    }

    var dimObject = new THREE.Object3D();
    dimObject.addChild(line);
    dimObject.addChild(arrow);
    dimObject.addChild(text);

    dimObject.rotation.z = angle;

    return dimObject;
}

SS.preview.createDimText = function(value) {
    var textGeo = new THREE.TextGeometry('' + value, {
	size: 2, height: 0.01, curveSegments: 6,
	font: 'helvetiker', weight: 'normal', style: 'normal',
	bezelEnabled: false});
    return new THREE.Mesh( textGeo, SS.preview.dimTextMaterial );
}

SS.preview.Ellipse1d = function(spec) {
    var that = this, scene = spec.scene, geomNode = spec.geomNode, updater = spec.updater;
    var cursoid = spec.cursoid;
    var anchorGeometry = new THREE.CubeGeometry(1.0, 1.0, 1.0);
    var anchorMaterial = new THREE.MeshBasicMaterial( { color: 0x66a1d1, opacity: 0.8, wireframe: false } );
    var sceneObjects = {};
    var activeAnchor;
    
    var updatePreview = function() {
        Object.keys(sceneObjects).map(function(key) {
            scene.removeObject(sceneObjects[key]);
        });
        sceneObjects = {};

        var r1 = geomNode.parameters.r1;
        var r2 = geomNode.parameters.r2;

	if (r1) {
	    sceneObjects.r1Dim = SS.preview.createDimArrow(r1, 0);
	}

	if (r2) {
	    sceneObjects.r2Dim = SS.preview.createDimArrow(r2, Math.PI/2);
	}

	var ellipseObj = new THREE.Object3D();

        if (r1 && r2) {
	    var ellipseGeom = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
	        var theta = Math.PI*2*i/50;
	        var dx = r1*Math.cos(theta);
	        var dy = r2*Math.sin(theta);
	        ellipseGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	    }
	    var ellipse = new THREE.Line(ellipseGeom, SS.constructors.lineMaterial);
            ellipseObj.addChild(ellipse);
        }

	if (activeAnchor !== 'origin') {
            var originAnchor = new THREE.Mesh(anchorGeometry, anchorMaterial);
            originAnchor.name = {anchor: 'origin'};
            ellipseObj.addChild(originAnchor);
	}

	if (activeAnchor !== 'radii') {
            var radiiAnchor = new THREE.Mesh(anchorGeometry, anchorMaterial);
            radiiAnchor.position.x = r1;
            radiiAnchor.position.y = r2;
            radiiAnchor.name = {anchor: 'radii'};
            ellipseObj.addChild(radiiAnchor);
	}
	
	sceneObjects.ellipse = ellipseObj;
    
	Object.keys(sceneObjects).map(function(key) {
	    sceneObjects[key].position.x = geomNode.origin.x;
	    sceneObjects[key].position.y = geomNode.origin.y;
	    sceneObjects[key].position.z = geomNode.origin.z;
	    scene.addObject(sceneObjects[key]);
	});

    }

    this.getAnchor = function(scene, camera, event) {
        var priorities = ['radii', 'origin'];

        var found = SS.selectInScene(scene, camera, event);
        var filtered = found.filter(function(obj) {
            return obj.object.name.anchor;
        });
	if (filtered.length == 0) {
	    return undefined;
	}
	var names = filtered.map(function(obj) {
	    return obj.object.name.anchor;
	});
	for (var key in priorities) {
	    if (names.indexOf(priorities[key]) !== -1) {
		return priorities[key];
	    }
	}
    };

    this.activateAnchor = function(anchorName) {
	var modifier;
	activeAnchor = anchorName;

	if (SS.modifier.active) {
	    SS.modifier.active.dispose();
	}
	if (anchorName === 'origin') {
	    modifier = new SS.modifier.Origin({geomNode : geomNode,
                                               cursoid  : sceneView.cursoid,
					       updater  : updater});
	    cursoid.setPosition(geomNode.origin);
	}
	if (anchorName === 'radii') {
	    modifier = new SS.modifier.MajorMinorRadii({geomNode : geomNode,
							cursoid  : sceneView.cursoid,
							updater  : updater});
	    var position = {x: geomNode.origin.x + geomNode.parameters.r1,
			    y: geomNode.origin.y + geomNode.parameters.r2,
			    z: geomNode.origin.z};
	    cursoid.setPosition(position);
	}

	if(modifier) {
	    SS.modifier.active = modifier;
	}
	updatePreview();
    }

    this.deactivateAnchor = function() {
	SS.modifier.active.dispose();
	SS.modifier.active == null;
    }

    this.dispose = function() {
	updater = null;
	cursoid.clear();
	Object.keys(sceneObjects).map(function(key) {
             scene.removeObject(sceneObjects[key]);
        });
    }
    
    spec.updater.on('updated', function(event) {
        updatePreview();
    });

    updatePreview();
}

SS.preview.createEllipse1d = function(spec) {
    
    var updater = new SS.GeomNodeUpdater(spec.geomNode);
    SS.constructors.active = new SS.preview.Ellipse1d({geomNode : spec.geomNode,
                                                      scene    : sceneView.scene,
                                                      cursoid  : sceneView.cursoid,
                                                      updater  : updater});

    
}

SS.constructors.disposeActive = function() {
    if (SS.constructors.active) {
	SS.constructors.active.dispose();
	SS.constructors.active = undefined;
    }
}
