var SS = SS || {};
SS.SceneView = function(container) {

    var camera, scene, renderer, w, h;
    var mesh, point;

    var overRenderer;

    var mouse = { x: 0, y: 0 }, mouseOnDown = { x: 0, y: 0 };
    var elevation = 0;
    var azimuth = Math.PI/4;

    var target = { azimuth: Math.PI/4, elevation: Math.PI*3/8 };
    var targetOnDown = { azimuth: target.azimuth, elevation: target.elevation };

    var distance = 1000, distanceTarget = 400;

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

	addGrid();

	/*var light = new THREE.PointLight(0xFFFFFF);
	light.position.set(1000, 1000, 0);
	scene.addLight(light);

	light = new THREE.PointLight(0xFFFFFF);
	light.position.set(1000, -1000, 1000);
	scene.addLight(light);

	light = new THREE.PointLight(0x666666);
	light.position.set(0, 0, -1000);
	scene.addLight(light);*/

	ambientLight = new THREE.AmbientLight( 0x101010 );
	scene.addLight( ambientLight );

	pointLight = new THREE.PointLight( 0xa0a050 );
	pointLight.position.z = 100;
	scene.addLight(pointLight);

	pointLight = new THREE.PointLight( 0x333333 );
	pointLight.position.z = -100;
	scene.addLight(pointLight);

	directionalLight = new THREE.DirectionalLight( 0xaaaa88 );
	directionalLight.position.x = 100;
	directionalLight.position.y = 50;
	directionalLight.position.z = 50;
	directionalLight.position.normalize();
	scene.addLight( directionalLight );

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

	var zoomDamp = Math.sqrt(distance)/10;

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

    function onWindowResize(event) {
	console.log('resize');
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

	camera.position.x = distance * Math.sin(elevation) * Math.cos(azimuth);
	camera.position.y = distance * Math.sin(elevation) * Math.sin(azimuth);
	camera.position.z = distance * Math.cos(elevation);

	renderer.clear();
	renderer.render(scene, camera);
    }
    
    var insideX = [-50,50];
    var insideY = [-50,50];
    var gridExtents = 100;
    var majorTick = 10;

    function addGrid() {

	var height = 0.01,
	size = 2,
	curveSegments = 6,
	font = "helvetiker", 		
	weight = "bold",		
	style = "normal";

	var textMaterial = new THREE.MeshBasicMaterial( { color: 0xffffff, opacity: 0.5, wireframe: false } );

	for (var x = (insideX[0]/majorTick); x <= (insideX[1]/majorTick); ++x) {
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
	    textMesh1.position.y = insideY[1] + 3;
	    textMesh1.position.x = x*10 + (x > 0 ? -2 : (x < 0 ? 2.5 : 0));
	    textMesh1.rotation.z = Math.PI;
	    scene.addObject(textMesh1);
	}

	for (var y = (insideY[0]/majorTick); y <= (insideY[1]/majorTick); ++y) {
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
	    textMesh1.position.x = insideY[1] + 3;
	    textMesh1.rotation.z = Math.PI/2;
	    scene.addObject(textMesh1);
	}

	addAxes();
	addMainGrid();
	for(var x = -gridExtents; x <= gridExtents; ++x) {
	    for(var y = -gridExtents; y <= gridExtents; ++y) {
		var inside = ((x >= insideX[0]) && (x <= insideX[1]) &&
			      (y >= insideY[0]) && (y <= insideY[1]));
		if ((x % majorTick == 0) && (y % majorTick == 0) &&
		    (x != 0) && (y != 0) &&
		    !inside) {
		    addFadingGridTile(x,y);
		}
	    }
	}

    }
    
    function addAxes() {

	axes = [new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry()];
	axes[0].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[0].vertices.push(new THREE.Vertex(new THREE.Vector3(500, 0, 0)));

	axes[1].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[1].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 500, 0)));

	axes[2].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[2].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 500)));

	axes[3].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[3].vertices.push(new THREE.Vertex(new THREE.Vector3(-gridExtents, 0, 0)));

	axes[4].vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
	axes[4].vertices.push(new THREE.Vertex(new THREE.Vector3(0, -gridExtents, 0)));

	scene.addObject(new THREE.Line(axes[0], new THREE.LineBasicMaterial({ color: 0x0000ff, opacity: 0.5 }))); //  X
	scene.addObject(new THREE.Line(axes[1], new THREE.LineBasicMaterial({ color: 0x00ff00, opacity: 0.5 }))); //  Y
	scene.addObject(new THREE.Line(axes[2], new THREE.LineBasicMaterial({ color: 0xff0000, opacity: 0.5 }))); //  Z
	scene.addObject(new THREE.Line(axes[3], new THREE.LineBasicMaterial({ color: 0x0000ff, opacity: 0.2 }))); // -X
	scene.addObject(new THREE.Line(axes[4], new THREE.LineBasicMaterial({ color: 0x00ff00, opacity: 0.2 }))); // -X
    }

    var majorGridLineGeometry = new THREE.Geometry();
    majorGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(insideX[0], 0, 0)));
    majorGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(insideX[1], 0, 0)));

    var minorGridLineGeometry = new THREE.Geometry();
    minorGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(insideX[0], 0, 0)));
    minorGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(insideX[1], 0, 0)));

    var majorMaterialInside = new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.5 });
    var minorMaterialInside = new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.05 });

    function addMainGrid() {


	for (var x = insideX[0]; x <= insideX[1]; ++x) {
	    if (x != 0) {
		var material = (x % 10 == 0) ? majorMaterialInside : minorMaterialInside;
		var geometry = (y % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
		var line = new THREE.Line(geometry, material);
		line.position.x = x;
		line.rotation.z = 90 * Math.PI / 180;
		scene.addObject(line);

	    }
	}

	for (var y = insideY[0]; y <= insideY[1]; ++y) {
	    if (y != 0) {
		var material = (y % 10 == 0) ? majorMaterialInside : minorMaterialInside;
		var geometry = (y % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
		var line = new THREE.Line(geometry, material);
		line.position.y = y;
		scene.addObject(line);
	    }
	}
    }
    
    var fadingGridLineGeometry = new THREE.Geometry();
    fadingGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
    fadingGridLineGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(majorTick, 0, 0)));
    
    function addFadingGridTile(x,y) {
	
	var dx = 0;
	if (x < insideX[0]) {
	    dx = insideX[0] - x;
	} else if (x > insideX[1]) {
	    dx = x - insideX[1];
	}
	var dy = 0;
	if (y < insideY[0]) {
	    dy = insideY[0] - y;
	} else if (y > insideY[1]) {
	    dy = y - insideY[1];
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
	    var geometry = createGeometry(geomNode.mesh);
	    var material = new THREE.MeshPhongMaterial( { ambient: 0x030303, color: 0x00dd00, specular: 0xccffcc, shininess: 100, shading: THREE.SmoothShading } );
	    //var material = new THREE.MeshLambertMaterial({ color: 0x00FF00 });
	    var mesh = new THREE.Mesh(geometry, material);
	    mesh.doubleSided = true;
	    scene.addObject(mesh);


        }
    }

    var remove = function(geomNode) {
        if (geomNode.path) {
        }
    }

    var createGeometry = function(mesh) {
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
	return geometry;
    }
    
    init();
    this.animate = animate;
    this.renderer = renderer;
    this.scene = scene;

    return this;
}

/*
function _SceneView() {

    this.eye = {x: 50.0, y: 0.0, z: 0.0};
    this.look = {x: 0.0, y:0.0, z:0.0};
    this.up = {z: 1.0};

    this.yaw_angle = -45;
    this.pitch_angle = 20;
    this.camera_translate = {x: 0, y:0, z:0};

    var add = function(geomNode) {
        if (geom_doc.isRoot(geomNode) && geomNode.mesh) {
            var sceneNode = {type : "geometry",
                             indices : geomNode.mesh.indices,
                             positions : geomNode.mesh.positions,
                             normals : geomNode.mesh.normals,
                             primitive : geomNode.mesh.primitive};

            SceneJS.withNode("geom").add("node", {type: "material",
                                                  id: geomNode.path,
                                                  emit: 0,
                                                  baseColor:      { r: 0.5, g: 1.0, b: 0.0 },
                                                  specularColor:  { r: 0.9, g: 0.9, b: 0.9 },
                                                  specular:       0.9,
                                                  shine:          100.0,
                                                  nodes: [sceneNode]});
            
            picker.addPickable(geomNode.path);
        }
    }

    var remove = function(geomNode) {
        if (geomNode.path 
            && 
            SceneJS.nodeExists(geomNode.path) 
            && 
            SceneJS.withNode(geomNode.path).parent()) {

            SceneJS.withNode(geomNode.path).parent().remove({node: geomNode.path});
        }
    }

    this.selectionUpdated = function(event) {
        if (event.deselected) {
            for (var i in event.deselected) {
                var path = event.deselected[i];
                SceneJS.withNode(path).set('baseColor', { r: 0.5, g: 1.0, b: 0.0 });
            }
        }
        if (event.selected) {
            for (var i in event.selected) {
                var path = event.selected[i];
                SceneJS.withNode(path).set('baseColor', { r: 1.0, g: 1.0, b: 0.0 });
            }
        }
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

}

var sceneView = new SceneView();

var scene = new SceneJS.Scene(
    {
        id: "theScene",
        canvasId: "theCanvas",
        loggingElementId: "theLoggingDiv"
    },
    new SceneJS.renderer(
	{ 
            clearColor : { r:0.01, g:0.01, b:0.01, a: 0 }, 
            clear : { depth : true, color : true} , 
            depthRange : { near: .5, far: 1500 }
	},
	new SceneJS.LookAt(
	    {
		id : "lookat",
		eye : sceneView.eye,
		look : sceneView.look,
		up : sceneView.up
            },
            new SceneJS.Camera(
		{
		    id: "camera",
                    type: "camera",
                    optics: {
			type: "perspective",
			fovy : 5.0,
			aspect : 1.47,
			near : 0.10,
			far : 3000.0
                    }
		},
		new SceneJS.Light(
                    {
			mode:                   "dir",
			color:                  { r: 0.5, g: 0.5, b: 0.5 },
			diffuse:                true,
			specular:               true,
			dir:                    { x: -1.0, y: -1.0, z: 1.0 }
                    }),
		new SceneJS.Light(
                    {
			mode:                   "dir",
			color:                  { r: 0.7, g: 0.7, b: 0.7 },
			diffuse:                true,
			specular:               true,
			dir:                    { x: 0.0, y: 1.0, z: -1.0 }
                    }),
		new SceneJS.Light(
                    {
			mode:                   "dir",
			color:                  { r: 0.8, g: 0.8, b: 0.8 },
			diffuse:                true,
			specular:               true,
			dir:                    { x: -1.0, y: -1.0, z: 1.0 }
                    }),
		new SceneJS.Translate(
                    {
			id : "cameraTranslate",
			x : sceneView.camera_translate.x,
			y : sceneView.camera_translate.y,
			z : sceneView.camera_translate.z
                    },
                    new SceneJS.Rotate(
			{
                            id: "pitch",
                            angle: sceneView.pitch_angle,
                            y : 1.0
			},
			new SceneJS.Rotate(
                            {
				id: "yaw",
				angle: sceneView.yaw_angle,
				z : 1.0
                            },
                            new SceneJS.Material(
				{
                                    id: "x-axis",
                                    emit: 1,
                                    baseColor:      { b: 1.0 },
                                    specular:       0,
				},
				new SceneJS.Geometry(
                                    {
					primitive: "lines",
					positions : [
                                            0,0,0,
                                            1000,0,0
					],
					normals : [],
					uv : [],
					uv2 : [],
					indices : [0,1]
                                    })),
                            new SceneJS.Material(
				{
                                    id: "y-axis",
                                    emit: 1,
                                    baseColor:      { g: 1.0 },
                                    specular:       0,
				},
				new SceneJS.Geometry(
                                    {
					primitive: "lines",
					positions : [
                                            0,0,0,
                                            0,1000,0
					],
					normals : [],
					uv : [],
					uv2 : [],
					indices : [0,1]
                                    })),
                            new SceneJS.Material(
				{
                                    id: "z-axis",
                                    emit: 1,
                                    baseColor:      { r: 1.0 },
                                    specular:       0,
				},
				new SceneJS.Geometry(
                                    {
					primitive: "lines",
					positions : [
                                            0,0,0,
                                            0,0,1000
					],
					normals : [],
					uv : [],
					uv2 : [],
					indices : [0,1]
                                    })),
                            new SceneJS.Material(
				{
                                    id: "geom",
                                    emit: 0,
                                    baseColor:      { r: 0.5, g: 1.0, b: 0.0 },
                                    specularColor:  { r: 0.9, g: 0.9, b: 0.9 },
                                    specular:       0.9,
                                    shine:          100.0,
				}
                            )
			)
                    )
		)
            )
	)
    )
);
              

window.render = function() {
    
    SceneJS.withNode("pitch").set("angle", sceneView.pitch_angle);
    SceneJS.withNode("yaw").set("angle", sceneView.yaw_angle);
    SceneJS.withNode("cameraTranslate").set("x", sceneView.camera_translate.x);
    SceneJS.withNode("cameraTranslate").set("y", sceneView.camera_translate.y);
    SceneJS.withNode("cameraTranslate").set("z", sceneView.camera_translate.z);

    SceneJS.withNode("theScene").render();
};


var pInterval;
SceneJS.bind("error", function() {
    window.clearInterval(pInterval);
});
SceneJS.bind("reset", function() {
    window.clearInterval(pInterval);
});
pInterval = window.setInterval("window.render()", 10);

function resetAspectRatio() {
    $('#theCanvas').width(window.innerWidth);
    $('#theCanvas').height(window.innerHeight);

    var aspect = window.innerWidth/window.innerHeight;
    var optics = SceneJS.withNode("camera").get("optics");
    optics.aspect = aspect;
    SceneJS.withNode("camera").set("optics", optics);
}

$(window).resize(function () { 
    resetAspectRatio();
});

*/




