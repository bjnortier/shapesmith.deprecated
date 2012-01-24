THREE.PipeGeometry = function (radius, positions) {

    THREE.Geometry.call( this );

    radius = radius !== undefined ? radius : 10;

    var segmentsX = 5;

    var x, y, vertices = [], uvs = [];

    var prand = new THREE.Vector3(Math.random(), Math.random(), Math.random());

    for ( y = 0; y < positions.length-1; y ++ ) {

	var verticesRow = [];
	var uvsRow = [];
        var v = y;

        var p1 = positions[y];
        var p2 = positions[y+1];
        var p1p2 = new THREE.Vector3().sub(p2, p1);

        var r = new THREE.Vector3().cross(p1p2, prand).normalize();
        var s = new THREE.Vector3().cross(p1p2, r).normalize();
        
	for ( x = 0; x <= segmentsX; x ++ ) {
            var u = x / segmentsX;
            var nx = r.x * Math.cos(u*Math.PI*2) + s.x*Math.sin(u*Math.PI);
            var ny = r.y * Math.cos(u*Math.PI*2) + s.y*Math.sin(u*Math.PI);
            var nz = r.z * Math.cos(u*Math.PI*2) + s.z*Math.sin(u*Math.PI);
            
            var vec1 = new THREE.Vector3(
                p1.x + r.x*Math.cos(u*Math.PI*2) + s.x*Math.sin(u*Math.PI*2),
                p1.y + r.y*Math.cos(u*Math.PI*2) + s.y*Math.sin(u*Math.PI*2),
                p1.z + r.z*Math.cos(u*Math.PI*2) + s.z*Math.sin(u*Math.PI*2));

	    this.vertices.push(new THREE.Vertex(vec1));
	    verticesRow.push(this.vertices.length - 1);
	    uvsRow.push( new THREE.UV( u, v ) );

            var u = x / segmentsX;
            var nx = r.x * Math.cos(u*Math.PI*2) + s.x*Math.sin(u*Math.PI);
            var ny = r.y * Math.cos(u*Math.PI*2) + s.y*Math.sin(u*Math.PI);
            var nz = r.z * Math.cos(u*Math.PI*2) + s.z*Math.sin(u*Math.PI);
            
            var vec2 = new THREE.Vector3(
                p2.x + r.x*Math.cos(u*Math.PI*2) + s.x*Math.sin(u*Math.PI*2),
                p2.y + r.y*Math.cos(u*Math.PI*2) + s.y*Math.sin(u*Math.PI*2),
                p2.z + r.z*Math.cos(u*Math.PI*2) + s.z*Math.sin(u*Math.PI*2));

	    this.vertices.push(new THREE.Vertex(vec2));
	    verticesRow.push(this.vertices.length - 1);
	    uvsRow.push( new THREE.UV( u, v ) );
        }

        for ( x = 0; x < segmentsX; x ++ ) {

            var v1 = verticesRow[x*2];
	    var v2 = verticesRow[x*2+1];
	    var v3 = verticesRow[x*2+3];
	    var v4 = verticesRow[x*2+2];

	    var n1 = this.vertices[ v1 ].position.clone().setY( 0 ).normalize();
	    var n2 = this.vertices[ v2 ].position.clone().setY( 0 ).normalize();
	    var n3 = this.vertices[ v3 ].position.clone().setY( 0 ).normalize();
	    var n4 = this.vertices[ v4 ].position.clone().setY( 0 ).normalize();

	    var uv1 = uvsRow[x*2].clone();
	    var uv2 = uvsRow[x*2+1].clone();
	    var uv3 = uvsRow[x*2+3].clone();
	    var uv4 = uvsRow[x*2+2].clone();

	    this.faces.push( new THREE.Face4( v1, v2, v3, v4, [ n1, n2, n3, n4 ] ) );
	    this.faceVertexUvs[ 0 ].push( [ uv1, uv2, uv3, uv4 ] );
        }

    }


    this.computeCentroids();
    this.computeFaceNormals();

}
THREE.PipeGeometry.prototype = new THREE.Geometry();
THREE.PipeGeometry.prototype.constructor = THREE.PipeGeometry;