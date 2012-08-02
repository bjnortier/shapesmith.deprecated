THREE.EllipseGeometry = function (r1, r2, theta1, theta2) {
    
    THREE.Geometry.call( this );
    
    r1 = r1 || 20;
    r2 = r2 || 10;
    theta1 = theta1 || 0;
    theta2 = theta2 || 360;
    var arcAngle = theta2 - theta1;
    arcAngle = arcAngle < 0 ? arcAngle + 360 : arcAngle; 

    var segments = segments || 20;

    this.vertices.push(new THREE.Vector3(0,0,0));
    for (var x = 0; x <= segments; x ++) {

        var dTheta = x/segments*arcAngle;

        var xpos = r1 * Math.cos((theta1 + dTheta)/180*Math.PI);
	var ypos = r2 * Math.sin((theta1 + dTheta)/180*Math.PI);
	var zpos = 0;
        
	this.vertices.push(new THREE.Vector3(xpos, ypos, zpos));

        if (x > 0) {
	    this.faces.push(new THREE.Face3(0, this.vertices.length - 2, this.vertices.length - 1));
        }
    }

    this.computeCentroids();
    this.computeFaceNormals();
    
}
THREE.EllipseGeometry.prototype = new THREE.Geometry();
THREE.EllipseGeometry.prototype.constructor = THREE.EllipseGeometry;
