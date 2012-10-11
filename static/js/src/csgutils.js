var SS = SS || {};

SS.toCSG = function(geomNode) {
    if (!geomNode.mesh) {
        throw 'no mesh for geom node ' + geomNode.id + ':' + geomNode.sha;
    }
    var mesh = geomNode.mesh;

    var i, pos, normal, vertex, index, index3, vertices;
    var polygons = [];
    for(i = 0; i < mesh.faces.indices.length/3; ++i) {

        vertices = [0,1,2].map(function(offset) {
            index = mesh.faces.indices[i*3+offset];
            index3 = index*3;
            var vectorConstructor =  CSG.Vector ||  CSG.Vector3D;
            pos = new vectorConstructor(
                mesh.faces.positions[index3], 
                mesh.faces.positions[index3+1], 
                mesh.faces.positions[index3+2]);
            normal = new vectorConstructor(
                mesh.faces.normals[index3], 
                mesh.faces.normals[index3+1], 
                mesh.faces.normals[index3+2]);
            return new CSG.Vertex(pos, normal);
        });
        if (!(vertices[0].pos.equals(vertices[1].pos) || vertices[1].pos.equals(vertices[2].pos) || vertices[0].pos.equals(vertices[2].pos))) {
            polygons.push(new CSG.Polygon(vertices));
        }
    }

    return new CSG.fromPolygons(polygons);
}   

SS.CSGtoMesh = function(csg) {
    var indices = [];
    var positions = [];
    var normals = [];

    var polygons = csg.toPolygons();
    var pos, normal, index;
    index = 0;
    polygons.forEach(function(polygon, polygonIndex) {
        var vertices = polygon.vertices;

        if (vertices.length < 3) {
            return;
        }
        var e1 = vertices[0].pos.minus(vertices[1].pos);
        var e2 = vertices[2].pos.minus(vertices[1].pos);
        var normal = e2.cross(e1).unit();

        // First vertex
        index0 = index;
        pos0 = vertices[0].pos;
        // normal = vertices[0].normal;
        positions.push(pos0.x);
        positions.push(pos0.y);
        positions.push(pos0.z);
        normals.push(normal.x);
        normals.push(normal.y);
        normals.push(normal.z);
        ++index;

        for (var i = 2; i < vertices.length; ++i) {
            indices.push(index0);

            pos1 = vertices[i-1].pos;
            // normal = vertices[i-1].normal;
            positions.push(pos1.x);
            positions.push(pos1.y);
            positions.push(pos1.z);
            normals.push(normal.x);
            normals.push(normal.y);
            normals.push(normal.z);
            indices.push(index);
            ++index;

            pos2 = vertices[i].pos;
            // normal = vertices[i].normal;
            positions.push(pos2.x);
            positions.push(pos2.y);
            positions.push(pos2.z);
            normals.push(normal.x);
            normals.push(normal.y);
            normals.push(normal.z);
            indices.push(index);
            ++index;
        }

    });
    return {
        faces: {
            indices: indices,
            normals: normals,
            positions: positions,
        },
        edges: {
            segments: []
        }
    }
}

