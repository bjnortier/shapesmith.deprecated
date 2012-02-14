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
    scene.add(pointer);
    
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

SS.workplane.grid = function(spec) {
    var that = {};
    var gridExtents = spec.gridExtents;

    var fadingGridLineGeometry = new THREE.Geometry();
    var majorGridLineGeometry = new THREE.Geometry();
    var minorGridLineGeometry = new THREE.Geometry();
    var majorMaterialInside = new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.2, transparent: true });
    var minorMaterialInside = new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.02, transparent: true });

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
		new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry()];
	axes[0].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[0].vertices.push(new THREE.Vertex(new THREE.Vector3(gridExtents.maxX + gridExtents.fadingWidth, 0, 0)));

	axes[1].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[1].vertices.push(new THREE.Vertex(new THREE.Vector3(0, gridExtents.maxY + gridExtents.fadingWidth, 0)));

	axes[2].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[2].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 500)));

	axes[3].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[3].vertices.push(new THREE.Vertex(new THREE.Vector3(gridExtents.minX - gridExtents.fadingWidth, 0, 0)));

	axes[4].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[4].vertices.push(new THREE.Vertex(new THREE.Vector3(0, gridExtents.minY - gridExtents.fadingWidth, 0)));

	axes[5].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[5].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, -500)));

	scene.add(new THREE.Line(axes[0], new THREE.LineBasicMaterial({ color: 0x0000ff, opacity: 0.5 }))); 
	scene.add(new THREE.Line(axes[1], new THREE.LineBasicMaterial({ color: 0x00ff00, opacity: 0.5 })));
	scene.add(new THREE.Line(axes[2], new THREE.LineBasicMaterial({ color: 0xff0000, opacity: 0.5 })));
	scene.add(new THREE.Line(axes[3], new THREE.LineBasicMaterial({ color: 0x0000ff, opacity: 0.2 })));
	scene.add(new THREE.Line(axes[4], new THREE.LineBasicMaterial({ color: 0x00ff00, opacity: 0.2 })));
	scene.add(new THREE.Line(axes[5], new THREE.LineBasicMaterial({ color: 0xff0000, opacity: 0.2 })));

    }

    var addMainGrid = function() {

	for (var x = gridExtents.minX; x <= gridExtents.maxX; ++x) {
	    if (x != 0) {
		var material = (x % 10 == 0) ? majorMaterialInside : minorMaterialInside;
		var geometry = (x % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
		var line = new THREE.Line(geometry, material);
		line.position.x = x;
	        line.position.z = (x % 10 == 0) ? line.position.z : line.position.z - 0.1;
		line.rotation.z = 90 * Math.PI / 180;
		scene.add(line);
	    }
	}

	for (var y = gridExtents.minY; y <= gridExtents.maxY; ++y) {
	    if (y != 0) {
		var material = (y % 10 == 0) ? majorMaterialInside : minorMaterialInside;
		var geometry = (y % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
		var line = new THREE.Line(geometry, material);
		line.position.y = y;
                line.position.z = (y % 10 == 0) ? line.position.z : line.position.z - 0.1;
		scene.add(line);
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
	scene.add(line);
	
	var line = new THREE.Line(fadingGridLineGeometry, material);
	line.position.x = x;
	line.position.y = y > 0 ? (y-majorTick) : y;
	line.rotation.z = 90 * Math.PI / 180;
	scene.add(line);

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
	    var text = new THREE.Mesh( textGeo, textMaterial );
	    text.position.y = gridExtents.maxY + 3;
	    text.position.x = x*10 + text.boundRadius/2;
	    text.rotation.z = Math.PI;
	    scene.add(text);
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
	    var text = new THREE.Mesh( textGeo, textMaterial );
            
	    text.position.y = y*10 - text.boundRadius/2;
	    text.position.x = gridExtents.maxX + 3;
	    text.rotation.z = Math.PI/2;
	    scene.add(text);
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

    addAxes();
    addMainGrid();
    addLabels();
    addFadingTiles();

    return that;

}

SS.Workplane = function(spec) {
    var that = {};

    var mouseOnWorkplane = {x: 0, y: 0}, scene = spec.scene;

    var gridExtents        = SS.workplane.gridExtents({minX: -60, minY: -60, maxX: 60, maxY: 60, fadingWidth: 50});
    var workplanePointer   = SS.workplane.pointer({scene: scene, gridExtents: gridExtents});
    var grid = SS.workplane.grid({scene: scene, gridExtents: gridExtents});

    _.extend(that, Backbone.Events);

    that.updateXYLocation = function(position, originalEvent) {

	if (position) {
	    var gridX = Math.round(position.x);
	    var gridY = Math.round(position.y);

	    // The reason for not limiting the events to only on the
	    // workplane extents, is that it breaks specifying the 
	    // height of e.g. cuboids
	    mouseOnWorkplane.x = gridX;
	    mouseOnWorkplane.y = gridY;
	    
	    workplanePointer.update({x: gridX, y: gridY});

	    that.trigger('workplaneXYCursorUpdated', {
		x: gridX, 
		y: gridY,
		originalEvent: originalEvent
	    });
	}
    }

    that.updateZLocation = function(position, originalEvent) {
        var gridZ = Math.round(position.z);
	that.trigger('workplaneZCursorUpdated', {
	    z: gridZ, 
	    originalEvent: originalEvent
	});
    }

    that.getPlaneMesh = function() {
	return grid.intersectionPlane;
    }
    
    that.getLastMousePosition = function() {
	return mouseOnWorkplane;
    }

    return that;
}

