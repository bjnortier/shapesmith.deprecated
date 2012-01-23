var SS = SS || {};
SS.preview = {};


SS.preview.dimTextMaterial 
    = new THREE.MeshBasicMaterial({ color: 0x66A1D2, opacity: 0.8, wireframe: false } );
SS.preview.dimLineMaterial 
    = new THREE.LineBasicMaterial({ color: 0x66A1D2, opacity: 0.5, wireframe : true });
SS.preview.arrowMaterial 
    = new THREE.MeshBasicMaterial({ color: 0x66A1D2, opacity: 0.5 });

SS.preview.createDimArrow = function(value, angle) {
    var r1DimensionGeom = new THREE.Geometry();
    r1DimensionGeom.vertices.push(
	new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
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
    var normalisedAngle = (angle + 2*Math.PI) % (2*Math.PI);
    if ((normalisedAngle > Math.PI/4) && (normalisedAngle < Math.PI*5/4)) {
	text.position.x = value/2 - text.boundRadius/2;
	text.position.y = 1;

    } else {
	text.position.x = value/2 + text.boundRadius/2;
	text.position.y = -1;
	text.rotation.z = Math.PI;
    }

    var dimObject = new THREE.Object3D();
    dimObject.add(line);
    dimObject.add(arrow);
    dimObject.add(text);

    dimObject.rotation.z = angle;

    return dimObject;
}

SS.preview.createAngleArrow = function(text, x, y, z, u, v, w, angle) {
    var angleObject = new THREE.Object3D();

    var angleGeometry = new THREE.Geometry(), r = 20;
    angleGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(1, 0, 0)));
    for (var i = 0; i < Math.floor(angle/360*72); ++i) {
        angleGeometry.vertices.push(
	    new THREE.Vertex(new THREE.Vector3(r*Math.cos(i*5/180*Math.PI),r* Math.sin(i*5/180*Math.PI), 0)));
    }
    angleGeometry.vertices.push(
        new THREE.Vertex(new THREE.Vector3(r*Math.cos(angle/180*Math.PI), r*Math.sin(angle/180*Math.PI), 0)));
    var angleLine = new THREE.Line(angleGeometry, SS.constructors.lineMaterial);
    angleObject.add(angleLine);


    var arrowGeometry = new THREE.Geometry();
    arrowGeometry.vertices.push(
	new THREE.Vertex(new THREE.Vector3(r, 0, 0)));
    arrowGeometry.vertices.push(
	new THREE.Vertex(new THREE.Vector3(r+1, -3, 0)));
    arrowGeometry.vertices.push(
	new THREE.Vertex(new THREE.Vector3(r-1, -3, 0)));
    
    var arrowFace = new THREE.Face3(0,1,2);
    arrowGeometry.faces.push(arrowFace);
    arrowGeometry.computeCentroids();
    arrowGeometry.computeFaceNormals();
    var arrow =  new THREE.Mesh(arrowGeometry, SS.preview.arrowMaterial);
    arrow.rotation.z = angle/180*Math.PI;
    arrow.doubleSided = true;
    angleObject.add(arrow);

    if (text) {
        var text = SS.preview.createDimText(text);
        text.position.x = (r+2)*Math.cos(angle/2/180*Math.PI);
        text.position.y = (r+2)*Math.sin(angle/2/180*Math.PI);
        text.rotation.z = Math.PI/2 + angle/2/180*Math.PI;
        angleObject.add(text);
    }

    angleObject.lookAt(new THREE.Vector3(u, v, w).normalize());
    return angleObject;
}

SS.preview.createDimArrow2 = function(text, u, v, w) {
    var r = parseFloat((Math.sqrt(u*u + v*v + w*w)).toFixed(3));

    var r1DimensionGeom = new THREE.Geometry();
    r1DimensionGeom.vertices.push(
	new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
    r1DimensionGeom.vertices.push(
	new THREE.Vertex(new THREE.Vector3(0, 0, r)));
    var line = new THREE.Line(r1DimensionGeom, SS.constructors.lineMaterial);
    
    var arrowGeometry = new THREE.Geometry();
    arrowGeometry.vertices.push(
	new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
    arrowGeometry.vertices.push(
	new THREE.Vertex(new THREE.Vector3(1, 0, -3)));
    arrowGeometry.vertices.push(
	new THREE.Vertex(new THREE.Vector3(-1, 0, -3)));
    
    var arrowFace = new THREE.Face3(0,1,2);
    arrowGeometry.faces.push(arrowFace);
    arrowGeometry.computeCentroids();
    arrowGeometry.computeFaceNormals();
    var arrow =  new THREE.Mesh(arrowGeometry, SS.preview.arrowMaterial);
    arrow.position.z = r;
    arrow.doubleSided = true;

    var dimObject = new THREE.Object3D();
    dimObject.add(line);
    dimObject.add(arrow);
    
    if (text) {
        var text = SS.preview.createDimText(text);
        if (u > 0) {
            text.lookAt(new THREE.Vector3(0, 1, 0).normalize());
        } else {
            text.lookAt(new THREE.Vector3(0, -1, 0).normalize());
        }
        text.position.z = r/2 + text.boundRadius/2;
        dimObject.add(text);
    }

    dimObject.lookAt(new THREE.Vector3(u, v, w).normalize());

    return dimObject;
}



SS.preview.createDimText = function(value) {
    var textGeo = new THREE.TextGeometry('' + value, {
	size: 2, height: 0.01, curveSegments: 6,
	font: 'helvetiker', weight: 'normal', style: 'normal',
	bezelEnabled: false});
    return new THREE.Mesh( textGeo, SS.preview.dimTextMaterial );
}

