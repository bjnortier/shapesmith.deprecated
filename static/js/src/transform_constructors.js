SS.constructors = SS.constructors || {};


SS.constructors.editTransform = function(geomNode, transform, specialisationClazz) {
    var updater = new SS.NodeUpdater(transform);
    SS.constructors.active = new SS.constructors.Constructor({geomNode : geomNode,
                                                              transform: transform,
                                                              scene    : sceneView.scene,
                                                              cursoid  : sceneView.cursoid,
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

    spec.transform.parameters.factor = 2.0;
    SS.constructors.editTranslate(spec.geomNode, spec.transform);
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

SS.constructors.Translate = function() {

    this.createSceneObject = function(geomNode, transform, activeAnchor) {
        
        var sceneObjects = {};
        
        var x = transform.origin.x, y = transform.origin.y, z = transform.origin.z;
        var u = transform.parameters.u, v = transform.parameters.v, w = transform.parameters.w;

	var translatedObj = new THREE.Object3D();
        var r = parseFloat((Math.sqrt(u*u + v*v + w*w)).toFixed(3));

        if (r > 0) {
            var rDim = SS.preview.createDimArrow2(r, u, v, w);
            sceneObjects.rDim = rDim;
        }

        if (r > 0) {

            var geometry = sceneView.createGeometry(geomNode.mesh);
	    var materials = [SS.constructors.faceMaterial];
	    var mesh = THREE.SceneUtils.createMultiMaterialObject(geometry, materials);

            mesh.position.x = u - x;
	    mesh.position.y = v - y;
	    mesh.position.z = w - z;
	    mesh.doubleSided = true;

	    translatedObj.add(mesh);
        }

        if (activeAnchor !== 'uvw') {
            var uvwAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            uvwAnchor.position.x = u;
            uvwAnchor.position.y = v;
            uvwAnchor.position.w = w;
            uvwAnchor.name = {anchor: 'uvw'};
            translatedObj.add(uvwAnchor);
	}

        sceneObjects.translated = translatedObj;
        return sceneObjects;
    }

    this.anchorPriorities = ['uvw', 'origin'];

    this.getModifierForAnchor = function(spec) {
        if (spec.anchorName === 'uvw') {
	    return new SS.modifier.UVW(spec);
	}
        return null;
    }

}

SS.constructors.Scale = function() {

    this.createSceneObject = function(geomNode, transform, activeAnchor) {
        
        var sceneObjects = {};
        
        var x = transform.origin.x, y = transform.origin.y, z = transform.origin.z;
        var factor = transform.parameters.factor;

	var scaledObj = new THREE.Object3D();
        if (factor) {

            var geometry = sceneView.createGeometry(geomNode.mesh);
            var originalPositions = geometry.vertices.map(function(vertex) {
	        return vertex.position;
            });

            var scaledGeometry = sceneView.createGeometry(geomNode.mesh);
	    scaledGeometry.vertices = originalPositions.map(function(position) {
	        var position = position.clone();
	        position.x = (position.x - x)*factor;
	        position.y = (position.y - y)*factor;
	        position.z = (position.z - z)*factor;
	        return new THREE.Vertex(position);
	    });
	    scaledGeometry.computeCentroids();
	    scaledGeometry.computeFaceNormals();

	    var materials = [SS.constructors.faceMaterial];
	    var mesh = THREE.SceneUtils.createMultiMaterialObject(scaledGeometry, materials);

	    scaledObj.add(mesh);
        }

        sceneObjects.scaled = scaledObj;
        return sceneObjects;
    }

    this.anchorPriorities = ['origin'];

    this.getModifierForAnchor = function(spec) {
        return null;
    }

}
