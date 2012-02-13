SS.constructors = SS.constructors || {};


SS.constructors.editTransform = function(geomNode, transform, specialisationClazz) {
    var updater = new SS.NodeUpdater(transform);
    SS.constructors.active = new SS.constructors.Constructor({geomNode : geomNode,
                                                              transform: transform,
                                                              scene    : SS.sceneView.scene,
                                                              cursoid  : SS.sceneView.cursoid,
                                                              specialisation : new specialisationClazz(),
                                                              updater  : updater});
}

SS.constructors.editTranslate = function(geomNode, transform) {
    SS.constructors.editTransform(geomNode, transform, SS.constructors.Translate);
}

SS.constructors.createTranslate = function(spec) {
    spec.transform.origin.x = 0;
    spec.transform.origin.y = 0;
    spec.transform.origin.z = 0;
    spec.transform.parameters.u = 10;
    spec.transform.parameters.v = 10;
    spec.transform.parameters.w = 0;
    spec.transform.parameters.n = 0;

    SS.constructors.editTranslate(spec.geomNode, spec.transform);
}

SS.constructors.editRotate = function(geomNode, transform) {
    SS.constructors.editTransform(geomNode, transform, SS.constructors.Rotate);
}

SS.constructors.createRotate = function(spec) {
    spec.transform.origin.x = 0;
    spec.transform.origin.y = 0;
    spec.transform.origin.z = 0;
    spec.transform.parameters.u = 0;
    spec.transform.parameters.v = 0;
    spec.transform.parameters.w = 1;
    spec.transform.parameters.angle = 45;
    spec.transform.parameters.n = 0;

    SS.constructors.editRotate(spec.geomNode, spec.transform);
}

SS.constructors.editScale = function(geomNode, transform) {
    SS.constructors.editTransform(geomNode, transform, SS.constructors.Scale);
}

SS.constructors.createScale = function(spec) {
    spec.transform.origin.x = 0;
    spec.transform.origin.y = 0;
    spec.transform.origin.z = 0;
    spec.transform.parameters.factor = 2.0;
    SS.constructors.editScale(spec.geomNode, spec.transform);
}

SS.constructors.editMirror = function(geomNode, transform) {
    SS.constructors.editTransform(geomNode, transform, SS.constructors.AxisMirror);
}

SS.constructors.createMirror = function(spec) {
    spec.transform.origin.x = 0;
    spec.transform.origin.y = 0;
    spec.transform.origin.z = 0;
    spec.transform.parameters.u = 0;
    spec.transform.parameters.v = 0;
    spec.transform.parameters.w = 10;
    spec.transform.parameters.n = 0;

    SS.constructors.editMirror(spec.geomNode, spec.transform);
}

SS.constructors.addGeometries = function(meshObject, geometries) {
    meshObject.add(geometries['faces']);
    meshObject.add(geometries['edges']);
}

SS.constructors.AxisMirror = function() {

    this.createSceneObject = function(geomNode, transform, activeAnchor) {
        
        var sceneObjects = {};
        
        var x = transform.origin.x, y = transform.origin.y, z = transform.origin.z;
        var u = transform.parameters.u, v = transform.parameters.v, w = transform.parameters.w;
        var angle = transform.parameters.angle;

	var mirrorObj = new THREE.Object3D();
        var r = parseFloat((Math.sqrt(u*u + v*v + w*w)).toFixed(3));

        if (r > 0) {
            var rDim = SS.preview.createDimArrow2('', u, v, w);
            sceneObjects.rDim = rDim;
        }

        if (r > 0) {

             var geometries = SS.renderGeometry(geomNode);
            for (key in geometries) {
                if (geometries[key].length > 0) {
                    geometries[key].map(function(geometry) {

                        var un, vn, wn;
                        var normUVW = new THREE.Vector3(u, v, w).normalize();
                        un = normUVW.x;
                        vn = normUVW.y;
                        wn = normUVW.z;

	                geometry.vertices = geometry.vertices.map(function(vertex) {
	                    var position = vertex.position.clone();
                            position.x = position.x - x;
                            position.y = position.y - y;
                            position.z = position.z - z;
                            
                            var a = (un*position.x + vn*position.y + wn*position.z);
                            var x2 = 2*a*un - position.x;
                            var y2 = 2*a*vn - position.y;
                            var z2 = 2*a*wn - position.z;
                            
	                    var newPosition = new THREE.Vector3(x2, y2, z2);
	                    return new THREE.Vertex(newPosition);
	                });
	                geometry.computeCentroids();
	                geometry.computeFaceNormals();                    
                    });
                }
            }


            var mirrorMeshObj = new THREE.Object3D();
            SS.constructors.addGeometries(mirrorMeshObj, geometries);
            sceneObjects.mirrorMeshObj = mirrorMeshObj;
        }

        if (activeAnchor !== 'uvw') {
            var uvwAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            uvwAnchor.position.x = u;
            uvwAnchor.position.y = v;
            uvwAnchor.position.z = w;
            uvwAnchor.name = {anchor: 'uvw'};
            mirrorObj.add(uvwAnchor);
	}

        sceneObjects.rotate = mirrorObj;
        return sceneObjects;
    }

    this.anchorPriorities = ['angle', 'uvw', 'origin'];

    this.getModifierForAnchor = function(spec) {
        if (spec.anchorName === 'uvw') {
	    return new SS.modifier.UVW(spec);
	}
        return null;
    }

}

