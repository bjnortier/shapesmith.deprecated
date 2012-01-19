var SS = SS || {};

SS.cursoid = {};

SS.cursoid.positionIndicator = function(value) {
    var that = {};
    
    var labelMesh = function(x) {
	var height = 0.01,
	size = 2,
	curveSegments = 6,
	font = "helvetiker", 		
	weight = "bold",		
	style = "normal";
	var labelMaterial = new THREE.MeshBasicMaterial( { color: 0xffff00, opacity: 0.7, wireframe: false } );
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
    background.position.y = 2;
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
	border.position.x = 0;
	border.position.y = 0.5;
	border.position.z = 0.15;
	indicatorBase.addChild(border);
    }

    var arrowGeometry = new THREE.Geometry();
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(-0.5, 0.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0.5, 0.5, 0)));
    var arrowFace = new THREE.Face3(0,1,2);
    arrowGeometry.faces.push(arrowFace);
    arrowGeometry.computeCentroids();
    arrowGeometry.computeFaceNormals();

    var arrowMaterial = new THREE.MeshBasicMaterial({ color: 0xe9fb00, opacity: 1.0 });
    var arrow = new THREE.Mesh(arrowGeometry, arrowMaterial);
    arrow.doubleSided = true;
    arrow.position.z = 0.15;

    indicatorBase.addChild(arrow);
    indicatorBase.addChild(background);

    var label = labelMesh(value);
    label = labelMesh(value);
    return {base: indicatorBase, label: label};
}

SS.cursoid.xPositionIndicator = function(x) {
    
    var baseAndLabel = SS.cursoid.positionIndicator(x);
    
    baseAndLabel.base.position.x = x;
    baseAndLabel.base.position.y = 0;
    baseAndLabel.base.rotation.z = Math.PI;

    baseAndLabel.label.position.y = 1;
    baseAndLabel.label.rotation.z = 0;

    baseAndLabel.base.addChild(baseAndLabel.label);
    return baseAndLabel.base;
}

SS.cursoid.yPositionIndicator = function(y) {
    
    var baseAndLabel = SS.cursoid.positionIndicator(y);
    
    baseAndLabel.base.position.x = 0;
    baseAndLabel.base.position.y = y;
    baseAndLabel.base.rotation.z = Math.PI/2;
    baseAndLabel.label.position.y = 3;

    baseAndLabel.base.addChild(baseAndLabel.label);
    return baseAndLabel.base;
}

SS.cursoid.zPositionIndicator = function(z) {
    
    var baseAndLabel = SS.cursoid.positionIndicator(z);
    
    baseAndLabel.base.position.x = 0;
    baseAndLabel.base.position.y = 0;
    baseAndLabel.base.position.z = z;
    baseAndLabel.base.rotation.y = Math.PI/2;
    baseAndLabel.label.position.y = 3;

    baseAndLabel.base.addChild(baseAndLabel.label);
    return baseAndLabel.base;
}


SS.Cursoid = function(spec) {

    var that = this, scene = spec.scene, position = {x:30, y:30, z:20}, cursoidSceneObject;
    var cursoidMaterial = new THREE.MeshBasicMaterial( { color: 0x66a1d1, opacity: 0.8, wireframe: false } );
    var cursoidXMaterial = new THREE.MeshBasicMaterial( { color: 0x000066, opacity: 0.8, wireframe: false } );
    var cursoidYMaterial = new THREE.MeshBasicMaterial( { color: 0x006600, opacity: 0.8, wireframe: false } );
    var cursoidXYMaterial = new THREE.MeshBasicMaterial( { color: 0x004444, opacity: 0.8, wireframe: false } );
    var cursoidZMaterial = new THREE.MeshBasicMaterial( { color: 0x660000, opacity: 0.8, wireframe: false } );
    var toXMaterial = new THREE.LineBasicMaterial({ color: 0x0000ff, opacity: 0.5, linewidth: 1 });
    var toYMaterial = new THREE.LineBasicMaterial({ color: 0x00ff00, opacity: 0.5, linewidth: 1 });
    var toZMaterial = new THREE.LineBasicMaterial({ color: 0xff0000, opacity: 0.5, linewidth: 1 })
    var zToZMaterial = new THREE.LineBasicMaterial({ color: 0x666666, opacity: 0.5, linewidth: 1 })
    var cursoidGeometry = new THREE.CubeGeometry(1.0, 1.0, 1.0);
    var modifier;

    evented(this);

    var updatePosition = function() {
        if (cursoidSceneObject) {
            scene.removeObject(cursoidSceneObject);
        }
        cursoidSceneObject = new THREE.Object3D();

        var toXLine1 = new THREE.Geometry(), toYLine1 = new THREE.Geometry();
        var toXLine2 = new THREE.Geometry(), toYLine2 = new THREE.Geometry();
        var toZLine1 = new THREE.Geometry(), toZLine2 = new THREE.Geometry(), toZLine3 = new THREE.Geometry();

	toXLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, 0, 0)));
	toXLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, position.y, 0)));
        cursoidSceneObject.addChild(new THREE.Line(toXLine1, toXMaterial));

	toYLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(0, position.y, 0)));
	toYLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, position.y, 0)));
        cursoidSceneObject.addChild(new THREE.Line(toYLine1, toYMaterial));

        if (position.z !== 0) {
	    toXLine2.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, position.z)));
	    toXLine2.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, 0, position.z)));
	    toXLine2.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, position.y, position.z)));
            //cursoidSceneObject.addChild(new THREE.Line(toXLine2, toXMaterial));
            
            toYLine2.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, position.z)));
	    toYLine2.vertices.push(new THREE.Vertex(new THREE.Vector3(0, position.y, position.z)));
	    toYLine2.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, position.y, position.z)));
            //cursoidSceneObject.addChild(new THREE.Line(toYLine2, toYMaterial));
	 
            //toZLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(0, position.y, 0)));
	    //toZLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(0, position.y, position.z)));
            //cursoidSceneObject.addChild(new THREE.Line(toZLine1, toZMaterial));

            toZLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, position.z)));
	    toZLine1.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, position.y, position.z)));
            cursoidSceneObject.addChild(new THREE.Line(toZLine1, toZMaterial));

            
            toZLine2.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, 0, 0)));
	    toZLine2.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, 0, position.z)));
            //cursoidSceneObject.addChild(new THREE.Line(toZLine2, toZMaterial));

            toZLine3.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, position.y, 0)));
	    toZLine3.vertices.push(new THREE.Vertex(new THREE.Vector3(position.x, position.y, position.z)));
            cursoidSceneObject.addChild(new THREE.Line(toZLine3, zToZMaterial));
        }


        cursoidSceneObject.addChild(SS.cursoid.xPositionIndicator(position.x));
        cursoidSceneObject.addChild(SS.cursoid.yPositionIndicator(position.y));
        if (position.z !== 0) {
            cursoidSceneObject.addChild(SS.cursoid.zPositionIndicator(position.z));
        }

        var cursoidPointer = new THREE.Mesh(cursoidGeometry, cursoidMaterial);
        cursoidPointer.position = position;
        if (position.z === 0) {
            cursoidPointer.name = {cursoid: 'xy'};
        }
        cursoidSceneObject.addChild(cursoidPointer);

        if (position.y !== 0) {
            var cursoidXPointer = new THREE.Mesh(cursoidGeometry, cursoidXMaterial);
            cursoidXPointer.position.x = position.x;
            cursoidXPointer.name = {cursoid: 'x'};
            cursoidSceneObject.addChild(cursoidXPointer)
        }

        if (position.x !== 0) {
            var cursoidYPointer = new THREE.Mesh(cursoidGeometry, cursoidYMaterial);
            cursoidYPointer.position.y = position.y;
            cursoidYPointer.name = {cursoid: 'y'};
            cursoidSceneObject.addChild(cursoidYPointer);
        }

        if ((position.x !== 0) && (position.y !== 0)) {
            var cursoidZPointer = new THREE.Mesh(cursoidGeometry, cursoidZMaterial);
            cursoidZPointer.position.z = position.z;
            cursoidZPointer.name = {cursoid: 'z'};
            cursoidSceneObject.addChild(cursoidZPointer);
        }

        if (position.z !== 0) {
            var cursoidXYPointer = new THREE.Mesh(cursoidGeometry, cursoidXYMaterial);
            cursoidXYPointer.position.x = position.x;
            cursoidXYPointer.position.y = position.y;
            cursoidXYPointer.name = {cursoid: 'xy'};
            cursoidSceneObject.addChild(cursoidXYPointer);
        }
        
        scene.addObject(cursoidSceneObject);
    };

    spec.workplane.on('workplaneXYCursorUpdated', function(event) {
        if (modifier && modifier.cursoid) {
            
            if (modifier.cursoid === 'xy') {
                position.x = event.x;
                position.y = event.y;
            } else if (modifier.cursoid === 'x') {
                position.x = event.x;
            } else if (modifier.cursoid === 'y') {
                position.y = event.y;
            } 
            updatePosition();
        }
    });

    spec.workplane.on('workplaneZCursorUpdated', function(event) {
        if (modifier && modifier.cursoid && (modifier.cursoid === 'z')) {
            position.z = event.z;
        }
        updatePosition();
    });
   

    this.getCursoid = function(scene, camera, event) {
        
        var found = SS.selectInScene(scene, camera, event);
        var filtered = found.filter(function(obj) {
            return obj.object.name.cursoid;
        });
        return filtered.length > 0 ? filtered[0].object.name.cursoid : undefined;
    };

    this.setCursoidModifier = function(cursoid) {
        modifier = {cursoid : cursoid};
    }

    this.resetModifier = function() {
        modifier = undefined;
    }

    updatePosition();
    return this;
    
};