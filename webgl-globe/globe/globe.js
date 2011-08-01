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

    colorFn = colorFn || function(x) {
	var c = new THREE.Color();
	c.setHSV( ( 0.6 - ( x * 0.5 ) ), 1.0, 1.0 );
	return c;
    };

    var camera, scene, sceneAtmosphere, renderer, w, h;
    var vector, mesh, atmosphere, point;

    var overRenderer;

    var zoomSpeed = 50;

    var mouse = { x: 0, y: 0 }, mouseOnDown = { x: 0, y: 0 };
    var elevation = 0;
    var azimuth = Math.PI/4;

    target = { azimuth: Math.PI/4, elevation: Math.PI*3/8 };
    targetOnDown = { azimuth: target.azimuth, elevation: target.elevation };

    var distance = 10000, distanceTarget = 1000;
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

	var geometry = new THREE.Sphere(50, 20, 20);
	var material = new THREE.MeshLambertMaterial( { color: 0xFF0000 } );
	var mesh = new THREE.Mesh(geometry, material);
	mesh.matrixAutoUpdate = false;
	scene.addObject(mesh);

	geometry = new THREE.Cube(200, 40, 30);
	material = new THREE.MeshLambertMaterial( { color: 0x00FF00, opacity: 0.5 } );
	mesh = new THREE.Mesh(geometry, material);
	mesh.matrixAutoUpdate = false;
	scene.addObject(mesh);

	geometry = new THREE.Geometry();
	geometry.vertices.push( new THREE.Vertex( new THREE.Vector3( - 500, 0, 0 ) ) );
	geometry.vertices.push( new THREE.Vertex( new THREE.Vector3( 500, 0, 0 ) ) );

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

	/*for ( var i = 0; i <= 200; i ++ ) {
	    
	    if (i % 20 != 0) {
		
		var line = new THREE.Line( geometry, new THREE.LineBasicMaterial( { color: 0xffffff, opacity: 0.15 } ) );
		line.position.z = ( i * 5 ) - 500;
		scene.addObject( line );
		
		var line = new THREE.Line( geometry, new THREE.LineBasicMaterial( { color: 0xffffff, opacity: 0.15 } ) );
		line.position.x = ( i * 5 ) - 500;
		line.rotation.y = 90 * Math.PI / 180;
		scene.addObject( line );
	    }
	    
	}*/

	/*for ( var i = -5; i <= 5; i ++ ) {
	    
	    var line = new THREE.Line( geometry, new THREE.LineBasicMaterial( { color: 0xffffff, opacity: 0.7 } ) );
	    line.position.z = ( i * 50 ) - 500;
	    scene.addObject( line );
	    
	    var line = new THREE.Line( geometry, new THREE.LineBasicMaterial( { color: 0xffffff, opacity: 0.7 } ) );
	    line.position.x = ( i * 50 ) - 500;
	    line.rotation.y = 90 * Math.PI / 180;
	    scene.addObject( line );
	    }*/
	
	/*var line = new THREE.Line( geometry, new THREE.LineBasicMaterial( { color: 0xffffff, opacity: 0.7 } ) );
	line.position.z = ( i * 500 ) - 500;
	scene.addObject( line );
	
	var line = new THREE.Line( geometry, new THREE.LineBasicMaterial( { color: 0x999999, opacity: 1.0 } ) );
	line.position.x = ( i * 500 ) - 500;
	line.rotation.y = 90 * Math.PI / 180;
	scene.addObject( line );*/

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

	var zoomDamp = distance/1000;

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
	    zoom(event.wheelDeltaY * 0.3);
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
	distanceTarget = distanceTarget > 1000 ? 1000 : distanceTarget;
	distanceTarget = distanceTarget < 350 ? 350 : distanceTarget;
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

    init();
    this.animate = animate;
    this.renderer = renderer;
    this.scene = scene;

    return this;

};

