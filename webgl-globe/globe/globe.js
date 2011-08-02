/**
 * dat.globe Javascript WebGL Globe Toolkit
 * http://dataarts.github.com/dat.globe
 *
 * Copyright 2011 Data Arts Team, Google Creative Lab
 *
 * Licensed under the Apache License, Version 2.0 (the 'License');
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 */

var DAT = DAT || {};

DAT.Globe = function(container, colorFn) {

    var camera, scene, sceneAtmosphere, renderer, w, h;
    var vector, mesh, atmosphere, point;

    var overRenderer;

    var mouse = { x: 0, y: 0 }, mouseOnDown = { x: 0, y: 0 };
    var elevation = 0;
    var azimuth = Math.PI/4;

    target = { azimuth: Math.PI/4, elevation: Math.PI*3/8 };
    targetOnDown = { azimuth: target.azimuth, elevation: target.elevation };

    var distance = 100, distanceTarget = 20;
    var padding = 40;
    var PI_HALF = Math.PI / 2;

    function init() {

	container.style.color = '#fff';
	container.style.font = '13px/20px Arial, sans-serif';

	var shader, uniforms, material;
	w = container.offsetWidth || window.innerWidth;
	h = container.offsetHeight || window.innerHeight;

	camera = new THREE.Camera(30, w / h, 1, 10000);
	camera.up.x = 0;
	camera.up.y = 0;
	camera.up.z = 1;
	
	vector = new THREE.Vector3();

	scene = new THREE.Scene();
	sceneAtmosphere = new THREE.Scene();

	/*var geometry = new THREE.Sphere(2.5, 20, 20);
	var material = new THREE.MeshLambertMaterial( { color: 0xFF0000 } );
	var mesh = new THREE.Mesh(geometry, material);
	mesh.matrixAutoUpdate = false;
	scene.addObject(mesh);

	geometry = new THREE.Cube(2, 1, 0.5);
	material = new THREE.MeshLambertMaterial( { color: 0x00FF00, opacity: 0.5 } );
	mesh = new THREE.Mesh(geometry, material);
	mesh.matrixAutoUpdate = false;
	scene.addObject(mesh);*/

	for(var tileX = -10; tileX <= 10; ++tileX) {
	    for(var tileY = -10; tileY <= 10; ++tileY) {
		addGridTile(tileX,tileY);
	    }
	}

	axes = [new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry()];
	axes[0].vertices.push( new THREE.Vertex( new THREE.Vector3( 00, 0, 0 ) ) );
	axes[0].vertices.push( new THREE.Vertex( new THREE.Vector3( 500, 0, 0 ) ) );

	axes[1].vertices.push( new THREE.Vertex( new THREE.Vector3( 0, 00, 0 ) ) );
	axes[1].vertices.push( new THREE.Vertex( new THREE.Vector3( -0, 500, 0 ) ) );

	axes[2].vertices.push( new THREE.Vertex( new THREE.Vector3( 0, 0, 00 ) ) );
	axes[2].vertices.push( new THREE.Vertex( new THREE.Vector3( 0, 0, 500 ) ) );

	scene.addObject(new THREE.Line( axes[0], new THREE.LineBasicMaterial( { color: 0x0000ff, opacity: 0.5 } ) ));
	scene.addObject(new THREE.Line( axes[1], new THREE.LineBasicMaterial( { color: 0x00ff00, opacity: 0.5 } ) ));
	scene.addObject(new THREE.Line( axes[2], new THREE.LineBasicMaterial( { color: 0xff0000, opacity: 0.5 } ) ));

	

	var light = new THREE.PointLight( 0xFFFF00 );
	light.position.set( -1000, 1000, 1000 );
	scene.addLight( light );

	renderer = new THREE.WebGLRenderer({antialias: true});
	renderer.autoClear = false;
	renderer.setClearColorHex(0x080808, 0.0);
	renderer.setSize(w, h);

	renderer.domElement.style.position = 'absolute';

	container.appendChild(renderer.domElement);

	container.addEventListener('mousedown', onMouseDown, false);
	container.addEventListener('mousewheel', onMouseWheel, false);
	document.addEventListener('keydown', onDocumentKeyDown, false);
	window.addEventListener('resize', onWindowResize, false);
	container.addEventListener('mouseover', function() {
	    overRenderer = true;
	}, false);
	container.addEventListener('mouseout', function() {
	    overRenderer = false;
	}, false);
    }


    function onMouseDown(event) {
	event.preventDefault();

	container.addEventListener('mousemove', onMouseMove, false);
	container.addEventListener('mouseup', onMouseUp, false);
	container.addEventListener('mouseout', onMouseOut, false);

	mouseOnDown.x = - event.clientX;
	mouseOnDown.y = event.clientY;

	targetOnDown.azimuth = target.azimuth;
	targetOnDown.elevation = target.elevation;

	container.style.cursor = 'move';
    }
    
    function onMouseMove(event) {
	mouse.x = - event.clientX;
	mouse.y = event.clientY;

	var zoomDamp = distance/10;

	target.azimuth = targetOnDown.azimuth + (mouse.x - mouseOnDown.x) * 0.005 * zoomDamp;
	target.elevation = targetOnDown.elevation - (mouse.y - mouseOnDown.y) * 0.005 * zoomDamp;

	target.elevation = target.elevation > Math.PI ? Math.PI : target.elevation;
	target.elevation = target.elevation < 0 ? 0 : target.elevation;
    }

    function onMouseUp(event) {
	container.removeEventListener('mousemove', onMouseMove, false);
	container.removeEventListener('mouseup', onMouseUp, false);
	container.removeEventListener('mouseout', onMouseOut, false);
	container.style.cursor = 'auto';
    }

    function onMouseOut(event) {
	container.removeEventListener('mousemove', onMouseMove, false);
	container.removeEventListener('mouseup', onMouseUp, false);
	container.removeEventListener('mouseout', onMouseOut, false);
    }

    function onMouseWheel(event) {
	event.preventDefault();
	if (overRenderer) {
	    zoom(event.wheelDeltaY * 0.05);
	}
	return false;
    }

    function onDocumentKeyDown(event) {
	switch (event.keyCode) {
	case 38:
            zoom(100);
            event.preventDefault();
            break;
	case 40:
            zoom(-100);
            event.preventDefault();
            break;
	}
    }

    function onWindowResize( event ) {
	console.log('resize');
	camera.aspect = window.innerWidth / window.innerHeight;
	camera.updateProjectionMatrix();
	renderer.setSize( window.innerWidth, window.innerHeight );
    }

    function zoom(delta) {
	distanceTarget -= delta;
	distanceTarget = distanceTarget > 100 ? 100 : distanceTarget;
	distanceTarget = distanceTarget < 3 ? 3 : distanceTarget;
    }

    function animate() {
	requestAnimationFrame(animate);
	render();
    }


    function render() {
	zoom(0);

	azimuth += (target.azimuth - azimuth) * 0.2;
	elevation += (target.elevation - elevation) * 0.3;
	distance += (distanceTarget - distance) * 0.3;

	camera.position.x = distance * Math.sin(elevation) * Math.cos(azimuth);
	camera.position.y = distance * Math.sin(elevation) * Math.sin(azimuth);
	camera.position.z = distance * Math.cos(elevation);

	renderer.clear();
	renderer.render(scene, camera);
	renderer.render(sceneAtmosphere, camera);
    }

    var gridLineGeometry = new THREE.Geometry();
    
    gridLineGeometry.vertices.push( new THREE.Vertex( new THREE.Vector3( 0, 0, 0 ) ) );
    gridLineGeometry.vertices.push( new THREE.Vertex( new THREE.Vector3( 1, 0, 0 ) ) );

    var majorMaterialInside = new THREE.LineBasicMaterial( { color: 0xffffff, opacity: 1 } );
    var minorMaterialInside = new THREE.LineBasicMaterial( { color: 0xffffff, opacity: 0.5 } );

    // Add a grid tile a position x,y where x and y are integer values
    // E.g. the first tile in the +x and +y quadrant is x=1, y=1
    // x=0 and y=0 has no meaning
    function addGridTile(x,y) {
	if ((x == 0) || (y == 0)) {
	    return;
	}

	var majorMaterial = majorMaterialInside;
	var inside = ((x >= -5) && (x <= 5) && (y >= -5) && (y <= 5));
	if (!inside) {
	    var opacity = 0.5;
	    majorMaterial = new THREE.LineBasicMaterial( { color: 0xffffff, opacity: opacity } );
	}
		
	var line = new THREE.Line(gridLineGeometry, majorMaterial);
	line.position.x = x > 0 ? (x-1) : x;
	line.position.y = y;
	scene.addObject(line);
	
	var line = new THREE.Line(gridLineGeometry, majorMaterial);
	line.position.x = x;
	line.position.y = y > 0 ? (y-1) : y;
	line.rotation.z = 90 * Math.PI / 180;
	scene.addObject( line );

	if (inside) {
	    for (var i = 1; i <= 9; i++) {
		
		var line = new THREE.Line(gridLineGeometry, minorMaterialInside);
		line.position.x = x > 0 ? (x-1) : x;
		line.position.y = (y > 0 ? (y-1) : y) + i/9;
		scene.addObject(line);
		
		var line = new THREE.Line(gridLineGeometry, minorMaterialInside);
		line.position.x = (x > 0 ? (x-1) : x) + i/9;
		line.position.y = y > 0 ? (y-1) : y;
		line.rotation.z = 90 * Math.PI / 180;
		scene.addObject(line);
		
	    }
	}
	
    }
    
    init();
    this.animate = animate;
    this.renderer = renderer;
    this.scene = scene;

    return this;

};

