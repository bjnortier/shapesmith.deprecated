var SS = SS || {};
SS.constructors = {};

SS.constructors.lineColor = 0x66A1D2;
SS.constructors.faceColor = 0x3F8FD2;
SS.constructors.faceMaterial = new THREE.MeshBasicMaterial( { color: SS.constructors.faceColor, transparent: true, opacity: 0.2 } );
SS.constructors.solidFaceMaterial = new THREE.MeshBasicMaterial( { color: SS.constructors.faceColor } );
SS.constructors.lineMaterial = new THREE.LineBasicMaterial({ color: SS.constructors.lineColor, wireframe : true });
SS.constructors.wireframeMaterial = new THREE.MeshBasicMaterial( { color: SS.constructors.lineColor, wireframe: true } )

SS.constructors.anchorGeometry = new THREE.CubeGeometry(1.0, 1.0, 1.0);
SS.constructors.anchorMaterial = new THREE.MeshBasicMaterial( { color: 0x66a1d1, opacity: 0.8, wireframe: false } );


SS.constructors.edit = function(geomNode, specialisation) {
    var updater = new SS.GeomNodeUpdater(geomNode);
    SS.constructors.active = new SS.constructors.Constructor({geomNode : geomNode,
                                                              scene    : sceneView.scene,
                                                              cursoid  : sceneView.cursoid,
                                                              specialisation : new specialisation(),
                                                              updater  : updater});
}

SS.constructors.editEllipse1d = function(geomNode) {
    SS.constructors.edit(geomNode, SS.constructors.Ellipse1d);
}

SS.constructors.createEllipse1d = function(geomNode) {
    var lastMousePosition = sceneView.workplane.getLastMousePosition();
    geomNode.origin.x = lastMousePosition.x;
    geomNode.origin.y = lastMousePosition.y;
    geomNode.parameters.r1 = 20;
    geomNode.parameters.r2 = 10;
    SS.constructors.editEllipse1d(geomNode);
}

SS.constructors.editCuboid = function(geomNode) {
    SS.constructors.edit(geomNode, SS.constructors.Cuboid);
}

SS.constructors.createCuboid = function(geomNode) {
    var lastMousePosition = sceneView.workplane.getLastMousePosition();
    geomNode.origin.x = lastMousePosition.x;
    geomNode.origin.y = lastMousePosition.y;
    geomNode.parameters.u = 10;
    geomNode.parameters.v = 10;
    geomNode.parameters.w = 10;
    SS.constructors.editCuboid(geomNode);
}

SS.constructors.editSphere = function(geomNode) {
    SS.constructors.edit(geomNode, SS.constructors.Sphere);
}

SS.constructors.createSphere = function(geomNode) {
    var lastMousePosition = sceneView.workplane.getLastMousePosition();
    geomNode.origin.x = lastMousePosition.x;
    geomNode.origin.y = lastMousePosition.y;
    geomNode.parameters.r = 10;
    SS.constructors.editSphere(geomNode);
}

SS.constructors.editCylinder = function(geomNode) {
    SS.constructors.edit(geomNode, SS.constructors.Cylinder);
}

SS.constructors.createCylinder = function(geomNode) {
    var lastMousePosition = sceneView.workplane.getLastMousePosition();
    geomNode.origin.x = lastMousePosition.x;
    geomNode.origin.y = lastMousePosition.y;
    geomNode.parameters.r = 10;
    geomNode.parameters.h = 20;
    SS.constructors.editCylinder(geomNode);
}

SS.constructors.editWedge = function(geomNode) {
    SS.constructors.edit(geomNode, SS.constructors.Wedge);
}

SS.constructors.createWedge = function(geomNode) {
    var lastMousePosition = sceneView.workplane.getLastMousePosition();
    geomNode.origin.x = lastMousePosition.x;
    geomNode.origin.y = lastMousePosition.y;
    geomNode.parameters.u1 = 20;
    geomNode.parameters.u2 = 10;
    geomNode.parameters.v = 10;
    geomNode.parameters.w = 10;
    SS.constructors.editWedge(geomNode);
}

SS.constructors.editTorus = function(geomNode) {
    SS.constructors.edit(geomNode, SS.constructors.Torus);
}

SS.constructors.createTorus = function(geomNode) {
    var lastMousePosition = sceneView.workplane.getLastMousePosition();
    geomNode.origin.x = lastMousePosition.x;
    geomNode.origin.y = lastMousePosition.y;
    geomNode.parameters.r1 = 20;
    geomNode.parameters.r2 = 5;
    SS.constructors.editTorus(geomNode);
}

SS.constructors.Constructor = function(spec) {
    var that = this, scene = spec.scene, geomNode = spec.geomNode, updater = spec.updater;
    var cursoid = spec.cursoid;

    var sceneObjects = {};
    var activeAnchor;
    var specialisation = spec.specialisation;
    
    var updatePreview = function() {
        Object.keys(sceneObjects).map(function(key) {
            scene.remove(sceneObjects[key]);
        });
        sceneObjects = specialisation.createSceneObject(geomNode, activeAnchor);

	if (activeAnchor !== 'origin') {
            var originAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            originAnchor.name = {anchor: 'origin'};
            sceneObjects.originAnchor = originAnchor;
	}
    
	Object.keys(sceneObjects).map(function(key) {
	    sceneObjects[key].position.x = geomNode.origin.x + sceneObjects[key].position.x;
	    sceneObjects[key].position.y = geomNode.origin.y + sceneObjects[key].position.y;
	    sceneObjects[key].position.z = geomNode.origin.z + sceneObjects[key].position.z;
            // Perfornamce boost as per
            // https://github.com/mrdoob/three.js/issues/167
            sceneObjects[key].children.map(function(child) {
                child.matrixAutoUpdate = false;
                child.updateMatrix();
            });
	    scene.add(sceneObjects[key]);
	});

    }

    this.getAnchor = function(scene, camera, event) {
        var priorities = specialisation.anchorPriorities;

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

        var spec = {geomNode  : geomNode,
                    cursoid   : sceneView.cursoid,
		    updater   : updater,
                    anchorName: anchorName};
        var modifier = specialisation.getModifierForAnchor(spec);
        if (!modifier && anchorName === 'origin') {
	    modifier = new SS.modifier.Origin({geomNode : geomNode,
                                               cursoid  : sceneView.cursoid,
					       updater  : updater});
	}

	if(modifier) {
	    SS.modifier.active = modifier;
	}
	updatePreview();
	return modifier.initialCursoidName;
    }

    this.deactivateAnchor = function() {
	SS.modifier.active.dispose();
	SS.modifier.active == null;
    }

    this.dispose = function() {
	cursoid.clear();
	Object.keys(sceneObjects).map(function(key) {
             scene.remove(sceneObjects[key]);
        });
        updater.off('updatedFromTree', updatePreview);
        updater.off('updatedFromCursoid', updatePreview);
        updater = null;
    }
    
    var init = function() {
        updater.on('updatedFromTree', updatePreview);
        updater.on('updatedFromCursoid', updatePreview);
        updatePreview();
    }
    
    init();
}


SS.constructors.disposeActive = function() {
    if (SS.constructors.active) {
	SS.constructors.active.dispose();
	SS.constructors.active = undefined;
    }
}

SS.constructors.Ellipse1d = function() {

    this.createSceneObject = function(geomNode, activeAnchor) {
        
        var sceneObjects = {};
        
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
            ellipseObj.add(ellipse);
        }
        
        if (activeAnchor !== 'radii') {
            var radiiAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            radiiAnchor.position.x = r1;
            radiiAnchor.position.y = r2;
            radiiAnchor.name = {anchor: 'radii'};
            ellipseObj.add(radiiAnchor);
	}

        sceneObjects.ellipse = ellipseObj;
        return sceneObjects;
    }

    this.anchorPriorities = ['radii', 'origin'];

    this.getModifierForAnchor = function(spec) {
        if (spec.anchorName === 'radii') {
	    return new SS.modifier.MajorMinorRadii(spec);
	}
        return null;
    }

}

SS.constructors.Cuboid = function() {

    this.createSceneObject = function(geomNode, activeAnchor) {
        
        var sceneObjects = {};
        
        var u = geomNode.parameters.u;
        var v = geomNode.parameters.v;
        var w = geomNode.parameters.w;
        if (u) {
	    sceneObjects.uDim = SS.preview.createDimArrow(u, 0);
	}
        if (v) {
	    sceneObjects.vDim = SS.preview.createDimArrow(v, Math.PI/2);
	}
        if (w) {
	    sceneObjects.wDim = SS.preview.createDimArrow(w, Math.PI);
            sceneObjects.wDim.rotation.y = Math.PI/2;
	}

	var cuboidObj = new THREE.Object3D();
        if(u &&v && w) {
            var geometry = new THREE.CubeGeometry(u,v,w);
	    var materials = [ SS.constructors.faceMaterial, SS.constructors.wireframeMaterial ];
	    var cube = THREE.SceneUtils.createMultiMaterialObject(geometry, materials);
	    cube.position.x = u/2;
	    cube.position.y = v/2;
	    cube.position.z = w/2;
	    cuboidObj.add(cube);
        } else if (u && v) {
            var uvLineGeom = new THREE.Geometry();
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    var uvLine = new THREE.Line(uvLineGeom, SS.constructors.lineMaterial);
	    cuboidObj.add(uvLine);
        }
        

        if (activeAnchor !== 'uv') {
            var uvAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            uvAnchor.position.x = u;
            uvAnchor.position.y = v;
            uvAnchor.name = {anchor: 'uv'};
            cuboidObj.add(uvAnchor);
	}

        if (activeAnchor !== 'w') {
            var wAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            wAnchor.position.z = w;
            wAnchor.name = {anchor: 'w'};
            cuboidObj.add(wAnchor);
	}

        sceneObjects.cuboid = cuboidObj;
        return sceneObjects;
    }

    this.anchorPriorities = ['uv', 'w', 'origin'];

    this.getModifierForAnchor = function(spec) {
        if (spec.anchorName === 'uv') {
	    return new SS.modifier.UV(spec);
	}
        if (spec.anchorName === 'w') {
	    return new SS.modifier.W(spec);
	}
        return null;
    }

}


SS.constructors.Wedge = function() {

    this.createSceneObject = function(geomNode, activeAnchor) {
        
        var sceneObjects = {};
        
        var u1 = geomNode.parameters.u1;
        var u2 = geomNode.parameters.u2;
        var v = geomNode.parameters.v;
        var w = geomNode.parameters.w;
        if (u1) {
	    sceneObjects.u1Dim = SS.preview.createDimArrow(u1, 0);
	}
        if (u2) {
	    sceneObjects.u2Dim = SS.preview.createDimArrow(u2, 0);
            sceneObjects.u2Dim.position.y = v;
	}
        if (v) {
	    sceneObjects.vDim = SS.preview.createDimArrow(v, Math.PI/2);
	}
        if (w) {
	    sceneObjects.wDim = SS.preview.createDimArrow(w, Math.PI);
            sceneObjects.wDim.rotation.y = Math.PI/2;
	}

	var wedgeObj = new THREE.Object3D();
        if (u1 && v && w) {
	    var geometry = new THREE.WedgeGeometry(u1,v,w,u2 - u1);
	    var materials = [ SS.constructors.faceMaterial, SS.constructors.wireframeMaterial ];
	    var wedge =  THREE.SceneUtils.createMultiMaterialObject(geometry, materials);
	    wedge.position.x = u1/2;
	    wedge.position.y = v/2;
	    wedge.position.z = w/2;
            wedgeObj.add(wedge);
        } else if (u1 && v && u2) {

	    var uvLineGeom = new THREE.Geometry();
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u1, 0, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(u2, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, v, 0)));
	    uvLineGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	    var uvLine = new THREE.Line(uvLineGeom, SS.constructors.lineMaterial);
	    wedgeObj(uvLine);
        }
        

        if (activeAnchor !== 'uv') {
            var uvAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            uvAnchor.position.x = u1;
            uvAnchor.position.y = v;
            uvAnchor.name = {anchor: 'uv'};
            wedgeObj.add(uvAnchor);
	}

        if (activeAnchor !== 'u2') {
            var wAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            wAnchor.position.x = u2;
            wAnchor.position.y = v;
            wAnchor.name = {anchor: 'u2'};
            wedgeObj.add(wAnchor);
	}

        if (activeAnchor !== 'w') {
            var wAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            wAnchor.position.z = w;
            wAnchor.name = {anchor: 'w'};
            wedgeObj.add(wAnchor);
	}

        sceneObjects.wedge = wedgeObj;
        return sceneObjects;
    }

    this.anchorPriorities = ['uv', 'w', 'u2', 'origin'];

    this.getModifierForAnchor = function(spec) {
        if (spec.anchorName === 'uv') {
            spec.uParameterName = 'u1';
	    return new SS.modifier.UV(spec);
	}
        if (spec.anchorName === 'u2') {
	    return new SS.modifier.U2(spec);
	}
        if (spec.anchorName === 'w') {
	    return new SS.modifier.W(spec);
	}
        return null;
    }

}

SS.constructors.Sphere = function() {

    this.createSceneObject = function(geomNode, activeAnchor) {
        
        var sceneObjects = {};
        
        var r = geomNode.parameters.r;
        if (r) {
	    sceneObjects.rDim = SS.preview.createDimArrow(r, (geomNode.extra && geomNode.extra.angle) || 0);
	}

	var sphereObj = new THREE.Object3D();
        if (r) {
            var geometry = new THREE.SphereGeometry(r, 50, 10);
	    var sphere = new THREE.Mesh(geometry, SS.constructors.faceMaterial);
	    sphereObj.add(sphere);

	    var circleGeom = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
	        var theta = Math.PI*2*i/50;
	        var dx = r*Math.cos(theta);
	        var dy = r*Math.sin(theta);
	        circleGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	    }
	    var circle = new THREE.Line(circleGeom, SS.constructors.lineMaterial);
	    sphereObj.add(circle);
        }

        if (activeAnchor !== 'radius') {
            var radiusAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            var angle = (geomNode.extra && geomNode.extra.angle) || 0;
            radiusAnchor.position.x = r*Math.cos(angle);
            radiusAnchor.position.y = r*Math.sin(angle);
            radiusAnchor.name = {anchor: 'radius'};
            sphereObj.add(radiusAnchor);
	}

        sceneObjects.sphere = sphereObj;
        return sceneObjects;
    }

    this.anchorPriorities = ['radius', 'origin'];

    this.getModifierForAnchor = function(spec) {
        if (spec.anchorName === 'radius') {
	    return new SS.modifier.Radius(spec);
	}
        return null;
    }

}

SS.constructors.Cylinder = function() {

    this.createSceneObject = function(geomNode, activeAnchor) {
        
        var sceneObjects = {};
        
        var r = geomNode.parameters.r;
        var h = geomNode.parameters.h;
        if (r) {
	    sceneObjects.rDim = SS.preview.createDimArrow(r, (geomNode.extra && geomNode.extra.angle) || 0);
	}
        if (h) {
	    sceneObjects.hDim = SS.preview.createDimArrow(h, Math.PI);
            sceneObjects.hDim.rotation.y = Math.PI/2;
	}

	var cylinderObj = new THREE.Object3D();
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
	    cylinderObj.add(circle1);
        }

        if (r && h) {
            var circle2 = new THREE.Line(circleGeom2, SS.constructors.lineMaterial);
	    circle2.position.z = h;
	    cylinderObj.add(circle2);

	    var geometry = new THREE.CylinderGeometry(r, r, h, 20);
	    var cylinder = new THREE.Mesh(geometry, SS.constructors.faceMaterial);
	    cylinder.position.z = h/2;
            cylinder.rotation.x = -Math.PI/2;
	    cylinderObj.add(cylinder);
        }

        if (activeAnchor !== 'radius') {
            var radiusAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            var angle = (geomNode.extra && geomNode.extra.angle) || 0;
            radiusAnchor.position.x = r*Math.cos(angle);
            radiusAnchor.position.y = r*Math.sin(angle);
            radiusAnchor.name = {anchor: 'radius'};
            cylinderObj.add(radiusAnchor);
	}

        if (activeAnchor !== 'height') {
            var heightAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            heightAnchor.position.z = h;
            heightAnchor.name = {anchor: 'height'};
            cylinderObj.add(heightAnchor);
	}

        sceneObjects.cylinder = cylinderObj;
        return sceneObjects;
    }

    this.anchorPriorities = ['height', 'radius', 'origin'];

    this.getModifierForAnchor = function(spec) {
        if (spec.anchorName === 'radius') {
	    return new SS.modifier.Radius(spec);
	}
        if (spec.anchorName === 'height') {
            spec.parameterName = 'h';
	    return new SS.modifier.W(spec);
	}
        return null;
    }

}


SS.constructors.Torus = function() {

    this.createSceneObject = function(geomNode, activeAnchor) {
        
        var sceneObjects = {};
        
        var r1 = geomNode.parameters.r1;
        var r2 = geomNode.parameters.r2;
        var angle = (geomNode.extra && geomNode.extra.angle) || 0;
        if (r1) {
	    sceneObjects.r1Dim = SS.preview.createDimArrow(r1, angle);
	}
        if (r1 && r2) {
	    sceneObjects.r2Dim = SS.preview.createDimArrow(r2, angle);
            sceneObjects.r2Dim.position.x = r1*Math.cos(angle);
            sceneObjects.r2Dim.position.y = r1*Math.sin(angle);
	}

	var torusObj = new THREE.Object3D();
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
	    torusObj.add(circle1);
	    torusObj.add(circle2);

	    var geometry = new THREE.TorusGeometry(r1, r2, 10, 50);
	    var torus = new THREE.Mesh(geometry, SS.constructors.faceMaterial);
	    torusObj.add(torus);
        }
        
        if (r1) {
	    var circleGeom = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
	        var theta = Math.PI*2*i/50;
	        var dx = r1*Math.cos(theta);
	        var dy = r1*Math.sin(theta);
	        circleGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	    }
	    var circle = new THREE.Line(circleGeom, SS.constructors.lineMaterial);
	    torusObj.add(circle);
        }

        if (activeAnchor !== 'r1') {
            var radiusAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            var angle = (geomNode.extra && geomNode.extra.angle) || 0;
            radiusAnchor.position.x = r1*Math.cos(angle);
            radiusAnchor.position.y = r1*Math.sin(angle);
            radiusAnchor.name = {anchor: 'r1'};
            torusObj.add(radiusAnchor);
	}

        if (activeAnchor !== 'r2') {
            var radiusAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            var angle = (geomNode.extra && geomNode.extra.angle) || 0;
            radiusAnchor.position.x = (r1+r2)*Math.cos(angle);
            radiusAnchor.position.y = (r1+r2)*Math.sin(angle);
            radiusAnchor.name = {anchor: 'r2'};
            torusObj.add(radiusAnchor);
	}

        sceneObjects.sphere = torusObj;
        return sceneObjects;
    }

    this.anchorPriorities = ['r1', 'r2', 'origin'];

    this.getModifierForAnchor = function(spec) {
        if (spec.anchorName === 'r1') {
            spec.parameterName = 'r1';
	    return new SS.modifier.Radius(spec);
	}
        if (spec.anchorName === 'r2') {
            spec.parameterName = 'r2';
	    return new SS.modifier.RadiusMinR1(spec);
	}
        return null;
    }

}
