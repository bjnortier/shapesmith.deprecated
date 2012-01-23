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
