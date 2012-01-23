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



SS.preview.createDimArrow2 = function(value, u, v, w) {
    var r1DimensionGeom = new THREE.Geometry();
    r1DimensionGeom.vertices.push(
	new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
    r1DimensionGeom.vertices.push(
	new THREE.Vertex(new THREE.Vector3(0, 0, value)));
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
    
    var arrowFace = new THREE.Face3(0,1,2);
    arrowGeometry.faces.push(arrowFace);
    arrowGeometry.computeCentroids();
    arrowGeometry.computeFaceNormals();
    var arrow =  new THREE.Mesh(arrowGeometry, SS.preview.arrowMaterial);
    arrow.position.z = value;
    arrow.doubleSided = true;
    
    var text = SS.preview.createDimText(value);

    if (u > 0) {
        text.lookAt(new THREE.Vector3(0, 1, 0).normalize());
    } else {
        text.lookAt(new THREE.Vector3(0, -1, 0).normalize());
    }
    text.position.z = value/2 + text.boundRadius/2;

    var dimObject = new THREE.Object3D();
    dimObject.add(line);
    dimObject.add(arrow);
    dimObject.add(text);
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

