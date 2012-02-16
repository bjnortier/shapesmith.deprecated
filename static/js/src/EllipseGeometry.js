THREE.EllipseGeometry = function (r1, r2, segments) {
    
    THREE.Geometry.call( this );
    
    r1 = r1 || 20;
    r2 = r2 || 10;

    var segments = segments || 20;

    this.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,0)));
    for (var x = 0; x <= segments; x ++) {

        var theta = x / segments;

        var xpos = r1 * Math.cos(theta * Math.PI*2);
	var ypos = r2 * Math.sin(theta * Math.PI*2);
	var zpos = 0;
        
	this.vertices.push(new THREE.Vertex(new THREE.Vector3(xpos, ypos, zpos)));

        if (x > 0) {
	    this.faces.push(new THREE.Face3(0, this.vertices.length - 2, this.vertices.length - 1));
        }
    }

    this.computeCentroids();
    this.computeFaceNormals();
    
}
THREE.EllipseGeometry.prototype = new THREE.Geometry();
THREE.EllipseGeometry.prototype.constructor = THREE.EllipseGeometry;
