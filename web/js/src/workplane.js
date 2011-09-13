var SS = SS || {};
SS.workplane = {};

SS.workplane.gridExtents = function(spec) {
    var that = {};

    that.minX = spec.minX;
    that.minY = spec.minY;
    that.maxX = spec.maxX;
    that.maxY = spec.maxY;
    that.fadingWidth = spec.fadingWidth;
    
    that.isInsideX = function(x) {
	return ((x >= that.minX) && (x <= that.maxX));
    }
    
    that.isInsideY = function(y) {
	return ((y >= that.minY) && (y <= that.maxY));
    }

    return that;
}

SS.workplane.pointer = function(spec) {
    var that = {};

    var scene = spec.scene, gridExtents = spec.gridExtents;
    var pointerMaterial = new THREE.MeshBasicMaterial( { color: 0xffff00, opacity: 0.7, wireframe: false } );
    var pointerGeometry = new THREE.CubeGeometry(0.5, 0.5, 0.5);
    var pointer = new THREE.Mesh(pointerGeometry, pointerMaterial);
    pointer.position.x = 0;
    pointer.position.y = 0;
    scene.addObject(pointer);
    
    that.update = function(position) {	
	if (gridExtents.isInsideX(position.x)) {
	    pointer.position.x = position.x;
	}
	if (gridExtents.isInsideY(position.y)) {
	    pointer.position.y = position.y;
	}
    }

    return that;
}

SS.workplane.positionIndicator = function(spec) {
    var that = {};
    
    var scene = spec.scene, gridExtents = spec.gridExtents;

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

    scene.addObject(indicatorBase);
    
    var label = labelMesh(0);
    scene.addObject(label);

    that.update = function(value) {
	scene.removeObject(label);
	label = labelMesh(value);
	scene.addObject(label);

	return {backdrop: indicatorBase, label: label};
    }

    return that;
}

SS.workplane.xPositionIndicator = function(spec) {
    var that = this.positionIndicator(spec);
    var superUpdate = that.update;
    
    var gridExtents = spec.gridExtents;

    that.update = function(x) {
	if (gridExtents.isInsideX(x)) {
	    var indicator = superUpdate(x);
	    indicator.backdrop.position.x = x;
	    indicator.backdrop.position.y = gridExtents.maxY;
	    indicator.label.position.x = x;
	    indicator.label.position.y = gridExtents.maxY + 3;
	}
    }

    return that;
}

SS.workplane.yPositionIndicator = function(spec) {
    var that = this.positionIndicator(spec);
    var superUpdate = that.update;

    var gridExtents = spec.gridExtents;

    that.update = function(y) {
	if (gridExtents.isInsideY(y)) {
	    var indicator = superUpdate(y);
	    indicator.backdrop.position.x = gridExtents.maxX;
	    indicator.backdrop.position.y = y;
	    indicator.backdrop.rotation.z = 3*Math.PI/2;
	    indicator.label.rotation.z = Math.PI/2;
	    indicator.label.position.y = y;
	    indicator.label.position.x = gridExtents.maxX + 3;
	}
    }

    return that;
}

SS.workplane.grid = function(spec) {
    var that = {};
    var gridExtents = spec.gridExtents;

    var fadingGridLineGeometry = new THREE.Geometry();
    var majorGridLineGeometry = new THREE.Geometry();
    var minorGridLineGeometry = new THREE.Geometry();
    var majorMaterialInside = new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.5 });
    var minorMaterialInside = new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.05 });

    var height = 0.01,
    majorTick = 10;
    size = 2,
    curveSegments = 6,
    font = "helvetiker", 		
    weight = "bold",		
    style = "normal",
    scene = spec.scene;

    majorGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(gridExtents.minX, 0, 0)));
    majorGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(gridExtents.maxX, 0, 0)));
    minorGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(gridExtents.minY, 0, 0)));
    minorGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(gridExtents.maxY, 0, 0)));
    fadingGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
    fadingGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(majorTick, 0, 0)));


    var addAxes = function() {

	axes = [new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry(), 
		new THREE.Geometry(), new THREE.Geometry()];
	axes[0].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[0].vertices.push(new THREE.Vertex(new THREE.Vector3(500, 0, 0)));

	axes[1].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[1].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 500, 0)));

	axes[2].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[2].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 500)));

	axes[3].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[3].vertices.push(new THREE.Vertex(new THREE.Vector3(gridExtents.minX - gridExtents.fadingWidth, 0, 0)));

	axes[4].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[4].vertices.push(new THREE.Vertex(new THREE.Vector3(0, gridExtents.minY - gridExtents.fadingWidth, 0)));

	scene.addObject(new THREE.Line(axes[0], new THREE.LineBasicMaterial({ color: 0x0000ff, opacity: 0.5 }))); 
	scene.addObject(new THREE.Line(axes[1], new THREE.LineBasicMaterial({ color: 0x00ff00, opacity: 0.5 })));
	scene.addObject(new THREE.Line(axes[2], new THREE.LineBasicMaterial({ color: 0xff0000, opacity: 0.5 })));
	scene.addObject(new THREE.Line(axes[3], new THREE.LineBasicMaterial({ color: 0x0000ff, opacity: 0.2 })));
	scene.addObject(new THREE.Line(axes[4], new THREE.LineBasicMaterial({ color: 0x00ff00, opacity: 0.2 })));
    }

    var addMainGrid = function() {

	for (var x = gridExtents.minX; x <= gridExtents.maxX; ++x) {
	    if (x != 0) {
		var material = (x % 10 == 0) ? majorMaterialInside : minorMaterialInside;
		var geometry = (y % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
		var line = new THREE.Line(geometry, material);
		line.position.x = x;
		line.rotation.z = 90 * Math.PI / 180;
		scene.addObject(line);

	    }
	}

	for (var y = gridExtents.minY; y <= gridExtents.maxY; ++y) {
	    if (y != 0) {
		var material = (y % 10 == 0) ? majorMaterialInside : minorMaterialInside;
		var geometry = (y % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
		var line = new THREE.Line(geometry, material);
		line.position.y = y;
		scene.addObject(line);
	    }
	}
    }
    
    
    var addFadingGridTile = function(x,y) {
	
	var dx = 0;
	if (x < gridExtents.minX) {
	    dx = gridExtents.minX - x;
	} else if (x > gridExtents.maxX) {
	    dx = x - gridExtents.maxX;
	}
	var dy = 0;
	if (y < gridExtents.minY) {
	    dy = gridExtents.minY - y;
	} else if (y > gridExtents.maxY) {
	    dy = y - gridExtents.maxY;
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

    var addLabels = function() {

	var textMaterial = new THREE.MeshBasicMaterial( { color: 0xffffff, opacity: 0.5, wireframe: false } );

	for (var x = (gridExtents.minX/majorTick); x <= (gridExtents.maxX/majorTick); ++x) {
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
	    textMesh1.position.y = gridExtents.maxY + 3;
	    textMesh1.position.x = x*10 + (x > 0 ? -2 : (x < 0 ? 2.5 : 0));
	    textMesh1.rotation.z = Math.PI;
	    scene.addObject(textMesh1);
	}

	for (var y = (gridExtents.minY/majorTick); y <= (gridExtents.maxY/majorTick); ++y) {
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
	    textMesh1.position.x = gridExtents.maxX + 3;
	    textMesh1.rotation.z = Math.PI/2;
	    scene.addObject(textMesh1);
	}
    }

    var addFadingTiles = function() {
	for(var x = gridExtents.minX - gridExtents.fadingWidth; 
	    x <= gridExtents.maxX + gridExtents.fadingWidth; 
	    ++x) {

	    for(var y = gridExtents.minY - gridExtents.fadingWidth; 
		y <= gridExtents.maxY + gridExtents.fadingWidth; 
		++y) {

		var inside = gridExtents.isInsideX(x) && gridExtents.isInsideY(y);
		
		if ((x % majorTick == 0) && (y % majorTick == 0) &&
		    (x != 0) && (y != 0) &&
		    !inside) {
		    addFadingGridTile(x,y);
		}
	    }
	}
    }

    var addIntersectionPlane = function() {
	var planeGeometry = new THREE.PlaneGeometry(1000, 1000);
	var planeMesh = new THREE.Mesh(planeGeometry,
				   new THREE.MeshBasicMaterial({ color: 0x080808, opacity: 0 }));
	planeMesh.doubleSided = true;
	scene.addObject(planeMesh);
	return planeMesh;
    }

    addAxes();
    addMainGrid();
    addLabels();
    addFadingTiles();
    that.intersectionPlane = addIntersectionPlane();

    return that;

}

SS.Workplane = function(scene) {


    var planeMesh, mouseOnWorkplane = {x: 0, y: 0};

    var gridExtents        = SS.workplane.gridExtents({minX: -50, minY: -50, maxX: 50, maxY: 50, fadingWidth: 50});
    var workplanePointer   = SS.workplane.pointer({scene: scene, gridExtents: gridExtents});
    var xPositionIndicator = SS.workplane.xPositionIndicator({scene: scene, gridExtents: gridExtents});
    var yPositionIndicator = SS.workplane.yPositionIndicator({scene: scene, gridExtents: gridExtents});
    var grid = SS.workplane.grid({scene: scene, gridExtents: gridExtents});

    this.updateXYLocation = function(x, y) {

	var gridX = Math.round(x);
	var gridY = Math.round(y);

	mouseOnWorkplane.x = gridX;
	mouseOnWorkplane.y = gridY;

	xPositionIndicator.update(gridX);
	yPositionIndicator.update(gridY);
	workplanePointer.update({x: gridX, y: gridY});
    }

    this.getPlaneMesh = function() {
	return grid.intersectionPlane;
    }
    
    this.getLastMousePosition = function() {
	return mouseOnWorkplane;
    }

}

