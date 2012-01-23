var SS = SS || {};

SS.cursoid = {};

SS.cursoid.positionIndicator = function(value, color) {
    var that = {};
    
    var labelMesh = function(x) {
	var height = 0.01,
	size = 2,
	curveSegments = 6,
	font = "helvetiker", 		
	weight = "bold",		
	style = "normal";
	var labelMaterial = new THREE.MeshBasicMaterial( { color: color, opacity: 1.0, wireframe: false } );
	var labelGeometry = new THREE.TextGeometry('' + Math.round(x), {
	    size: size, 
	    height: height,
	    curveSegments: curveSegments,
	    font: font,
	    weight: weight,
	    style: style,
	    bezelEnabled: false
	});
	var label = new THREE.Mesh(labelGeometry, labelMaterial);


	label.position.z = 0.1;
	label.rotation.z = Math.PI;
	return label;
    }

    var indicatorBase = new THREE.Object3D();
    var background = new THREE.Mesh(new THREE.PlaneGeometry(5, 3),
				    new THREE.MeshBasicMaterial({ color: 0x101010, opacity: 1.0 }));
    background.position.x = 0;
    background.position.y = 0;
    background.position.z = 0.1;
    
    var borderGeom = [new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry()];
    borderGeom[0].vertices.push(new THREE.Vertex(new THREE.Vector3(-2.5, -1.5, 0)));
    borderGeom[0].vertices.push(new THREE.Vertex(new THREE.Vector3(2.5, -1.5, 0)));
    borderGeom[1].vertices.push(new THREE.Vertex(new THREE.Vector3(2.5, -1.5, 0)));
    borderGeom[1].vertices.push(new THREE.Vertex(new THREE.Vector3(2.5, 1.5, 0)));
    borderGeom[2].vertices.push(new THREE.Vertex(new THREE.Vector3(2.5, 1.5, 0)));
    borderGeom[2].vertices.push(new THREE.Vertex(new THREE.Vector3(-2.5, 1.5, 0)));
    borderGeom[3].vertices.push(new THREE.Vertex(new THREE.Vector3(-2.5, 1.5, 0)));
    borderGeom[3].vertices.push(new THREE.Vertex(new THREE.Vector3(-2.5, -1.5, 0)));
    for (var i = 0; i < 4; ++i) {
	var border = new THREE.Line(borderGeom[i], new THREE.LineBasicMaterial({ color: color, opacity: 0.5 }));
	border.position.x = 0;
	border.position.y = 0;
	border.position.z = 0.15;
	indicatorBase.add(border);
    }

    indicatorBase.add(background);

    var label = labelMesh(value);
    label = labelMesh(value);
    return {base: indicatorBase, label: label};
}

SS.cursoid.xPositionIndicator = function(x) {
    
    var baseAndLabel = SS.cursoid.positionIndicator(x, 0x6666ff);
    
    baseAndLabel.base.position.x = x;
    baseAndLabel.base.position.y = -2;
    baseAndLabel.base.rotation.z = Math.PI;

    baseAndLabel.label.position.y = -1;
    baseAndLabel.label.position.x = -baseAndLabel.label.boundRadius/2;
    baseAndLabel.label.rotation.z = 0;

    baseAndLabel.base.add(baseAndLabel.label);
    return baseAndLabel.base;
}

SS.cursoid.yPositionIndicator = function(y) {
    
    var baseAndLabel = SS.cursoid.positionIndicator(y, 0x66ff66);
    
    baseAndLabel.base.position.x = -2;
    baseAndLabel.base.position.y = y;
    baseAndLabel.base.rotation.z = Math.PI/2;
    baseAndLabel.label.position.y = -1;
    baseAndLabel.label.position.x = -baseAndLabel.label.boundRadius/2;
    baseAndLabel.label.rotation.z = 0;

    baseAndLabel.base.add(baseAndLabel.label);
    return baseAndLabel.base;
}

SS.cursoid.zPositionIndicator = function(z) {
    
    var baseAndLabel = SS.cursoid.positionIndicator(z, 0xff6666);
    
    baseAndLabel.base.position.x = -3;
    baseAndLabel.base.position.y = 0;
    baseAndLabel.base.position.z = z;
    baseAndLabel.base.rotation.x = -Math.PI/2;
    baseAndLabel.label.position.y = 1.0;
    baseAndLabel.label.position.x = baseAndLabel.label.boundRadius/2;

    baseAndLabel.base.add(baseAndLabel.label);
    return baseAndLabel.base;
}


SS.Cursoid = function(spec) {

    var that = this, scene = spec.scene, position, cursoidSceneObject;
    var cursoidMaterial = new THREE.MeshBasicMaterial( { color: 0xffffff, opacity: 0.8, wireframe: false } );
    var cursoidXMaterial = new THREE.MeshBasicMaterial( { color: 0x6666ff, opacity: 0.8, wireframe: false } );
    var cursoidYMaterial = new THREE.MeshBasicMaterial( { color: 0x66ff66, opacity: 0.8, wireframe: false } );
    var cursoidXYMaterial = new THREE.MeshBasicMaterial( { color: 0x66dddd, opacity: 0.8, wireframe: false } );
    var cursoidZMaterial = new THREE.MeshBasicMaterial( { color: 0xff6666, opacity: 0.8, wireframe: false } );
    var toXMaterial = new THREE.LineBasicMaterial({ color: 0x0000ff, opacity: 0.5, linewidth: 1 });
    var toYMaterial = new THREE.LineBasicMaterial({ color: 0x00ff00, opacity: 0.5, linewidth: 1 });
    var toZMaterial = new THREE.LineBasicMaterial({ color: 0xff0000, opacity: 0.5, linewidth: 1 })
    var zToZMaterial = new THREE.LineBasicMaterial({ color: 0x666666, opacity: 0.5, linewidth: 1 })
    var cursoidGeometry = new THREE.CubeGeometry(1.0, 1.0, 1.0);
    var active, exclusions = [];

    evented(this);

    var updatePosition = function() {
        if (cursoidSceneObject) {
            scene.remove(cursoidSceneObject);
        }
        cursoidSceneObject = new THREE.Object3D();

        var toXLine1 = new THREE.Geometry(), toYLine1 = new THREE.Geometry();
        var toXLine2 = new THREE.Geometry(), toYLine2 = new THREE.Geometry();
        var toZLine1 = new THREE.Geometry(), toZLine2 = new THREE.Geometry();

	toXLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, 0, 0)));
	toXLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, position.y, 0)));
        cursoidSceneObject.add(new THREE.Line(toXLine1, toXMaterial));

	toYLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(0, position.y, 0)));
	toYLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, position.y, 0)));
        cursoidSceneObject.add(new THREE.Line(toYLine1, toYMaterial));

        if (position.z !== 0) {
            toZLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, position.z)));
	    toZLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, position.y, position.z)));
            cursoidSceneObject.add(new THREE.Line(toZLine1, toZMaterial));

            toZLine2.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, position.y, 0)));
	    toZLine2.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, position.y, position.z)));
            cursoidSceneObject.add(new THREE.Line(toZLine2, zToZMaterial));
        }


        cursoidSceneObject.add(SS.cursoid.xPositionIndicator(position.x));
        cursoidSceneObject.add(SS.cursoid.yPositionIndicator(position.y));
        if (position.z !== 0) {
            cursoidSceneObject.add(SS.cursoid.zPositionIndicator(position.z));
        }

        var cursoidPointer = new THREE.Mesh(cursoidGeometry, cursoidMaterial);
        cursoidPointer.position = position;
        if (position.z === 0) {
            cursoidPointer.name = {cursoid: 'xy'};
        }
        cursoidSceneObject.add(cursoidPointer);

        if ((position.x !== 0) && (position.y !== 0) && (exclusions.indexOf('x') === -1)) {
            var cursoidXPointer = new THREE.Mesh(cursoidGeometry, cursoidXMaterial);
            cursoidXPointer.position.x = position.x;
            cursoidXPointer.name = {cursoid: 'x'};
            cursoidSceneObject.add(cursoidXPointer)
        }

        if ((position.x !== 0) && (position.y !== 0) && (exclusions.indexOf('y') === -1)) {
            var cursoidYPointer = new THREE.Mesh(cursoidGeometry, cursoidYMaterial);
            cursoidYPointer.position.y = position.y;
            cursoidYPointer.name = {cursoid: 'y'};
            cursoidSceneObject.add(cursoidYPointer);
        }

        if (exclusions.indexOf('z') === -1) {
            var cursoidZPointer = new THREE.Mesh(cursoidGeometry, cursoidZMaterial);
            cursoidZPointer.position.z = position.z;
            cursoidZPointer.name = {cursoid: 'z'};
            cursoidSceneObject.add(cursoidZPointer);
        }

        if ((position.z !== 0)  && (exclusions.indexOf('xy') === -1)) {
            var cursoidXYPointer = new THREE.Mesh(cursoidGeometry, cursoidXYMaterial);
            cursoidXYPointer.position.x = position.x;
            cursoidXYPointer.position.y = position.y;
            cursoidXYPointer.name = {cursoid: 'xy'};
            cursoidSceneObject.add(cursoidXYPointer);
        }
        
        scene.add(cursoidSceneObject);

    };

    spec.workplane.on('workplaneXYCursorUpdated', function(event) {
        if (active) {

            if (active === 'xy') {
                position.x = event.x;
                position.y = event.y;
            } else if (active === 'x') {
                position.x = event.x;
            } else if (active === 'y') {
                position.y = event.y;
            } 
            updatePosition();
            that.fire({type: 'cursoidUpdated', 
		   position: {x: position.x, y: position.y, z: position.z}});

        }
    });

    spec.workplane.on('workplaneZCursorUpdated', function(event) {
        if (active && (active === 'z')) {
            position.z = event.z;
            updatePosition();
            that.fire({type: 'cursoidUpdated', 
		       position: {x: position.x, y: position.y, z: position.z}});

        }
    });
   

    this.getCursoid = function(scene, camera, event) {
        
        var found = SS.selectInScene(scene, camera, event);
        var filtered = found.filter(function(obj) {
            return obj.object.name.cursoid;
        });
        return filtered.length > 0 ? filtered[0].object.name.cursoid : undefined;
    };

    this.activate = function(cursoid) {
        active = cursoid;
    }

    this.deactivate = function() {
        active = undefined;
    }

    this.clear = function() {
	this.deactivate();
	if (cursoidSceneObject) {
	    scene.remove(cursoidSceneObject);
	}
    }

    this.setPosition = function(newPosition) {
        this.setPositionAndExclusions(newPosition, []);
    }

    this.setPositionAndExclusions = function(newPosition, newExclusions) {
        exclusions = newExclusions || [];
        position = {x: newPosition.x,
                    y: newPosition.y,
                    z: newPosition.z};
        updatePosition();
    }

    return this;
    
};