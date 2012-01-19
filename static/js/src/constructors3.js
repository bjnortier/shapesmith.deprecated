var SS = SS || {};
SS.constructors = {};
SS.preview = {};
SS.modifier = {};

SS.GeomNodeUpdater = function(geomNode) {
    var geomNode = geomNode, that = this;

    evented(this);

    this.setOrigin = function(origin) {
        geomNode.origin = origin;
        that.fire({type: 'updated'});
    }


    this.setR1R2 = function(r1r2) {
	geomNode.parameters.r1 = r1r2.r1;
	geomNode.parameters.r2 = r1r2.r2;
        that.fire({type: 'updated'});
    }

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

SS.preview.Ellipse1d = function(spec) {
    var that = this, scene = spec.scene, geomNode = spec.geomNode, updater = spec.updater;
    var cursoid = spec.cursoid;
    var anchorGeometry = new THREE.CubeGeometry(1.0, 1.0, 1.0);
    var anchorMaterial = new THREE.MeshBasicMaterial( { color: 0x66a1d1, opacity: 0.8, wireframe: false } );
    var sceneObject;
    var activeAnchor;
    
    var updatePreview = function() {
        if(sceneObject) {
            scene.removeObject(sceneObject);
        }
        sceneObject = new THREE.Object3D();

        var r1 = geomNode.parameters.r1;
        var r2 = geomNode.parameters.r2;

        if (r1 && r2) {

	    var ellipseGeom = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
	        var theta = Math.PI*2*i/50;
	        var dx = r1*Math.cos(theta);
	        var dy = r2*Math.sin(theta);
	        ellipseGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	    }
	    var ellipse = new THREE.Line(ellipseGeom, SS.constructors.lineMaterial);
            sceneObject.addChild(ellipse);
        }

	if (activeAnchor !== 'origin') {
            var originAnchor = new THREE.Mesh(anchorGeometry, anchorMaterial);
            originAnchor.name = {anchor: 'origin'};
            sceneObject.addChild(originAnchor);
	}

	if (activeAnchor !== 'radii') {
            var radiiAnchor = new THREE.Mesh(anchorGeometry, anchorMaterial);
            radiiAnchor.position.x = r1;
            radiiAnchor.position.y = r2;
            radiiAnchor.name = {anchor: 'radii'};
            sceneObject.addChild(radiiAnchor);
	}
    
        sceneObject.position.x = geomNode.origin.x;
        sceneObject.position.y = geomNode.origin.y;
        sceneObject.position.z = geomNode.origin.z;
    
        scene.addObject(sceneObject);
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
    
    spec.updater.on('updated', function(event) {
        updatePreview();
    });

    updatePreview();
}

SS.preview.createEllipse1d = function(spec) {
    
    var updater = new SS.GeomNodeUpdater(spec.geomNode);
    SS.constructor.active = new SS.preview.Ellipse1d({geomNode : spec.geomNode,
                                                      scene    : sceneView.scene,
                                                      cursoid  : sceneView.cursoid,
                                                      updater  : updater});

    
}

SS.constructors.disposeActive = function() {
    SS.constructors.active = undefined;
}
