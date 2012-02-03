var SS = SS || {};
SS.transformers = {};

SS.transformers.Manager = function() {

    var that = this;
    var transformerUI = new THREE.Object3D();
    var UISTATE = {DEACTIVATED: 0, SCALE: 1, ROTATE: 2};
    var uiState = UISTATE.DEACTIVATED;
    var translatingState = undefined;
    var lastWorkplanePosition = undefined;

    var centerOfGeom = function(boundingBox) {
        return new THREE.Vector3().add(boundingBox.min, 
                                       new THREE.Vector3().sub(boundingBox.max, boundingBox.min).divideScalar(2));
    }

    var uiElements = [];

    var activateScale = function(geomNode) {
        uiElements = [];
        SS.sceneView.scene.remove(transformerUI);

        transformerUI = new THREE.Object3D();
        var boundingBox = SS.boundingBoxForGeomNode(geomNode);
        
        var width = boundingBox.max.x - boundingBox.min.x;
        var depth = boundingBox.max.y - boundingBox.min.y;
        var height = boundingBox.max.z - boundingBox.min.z;

        var geometry = new THREE.CubeGeometry(width, depth, height);
	var materials = [SS.constructors.wireframeMaterial];
	var cube = THREE.SceneUtils.createMultiMaterialObject(geometry, materials);

	cube.position.x = boundingBox.min.x + width/2;
	cube.position.y = boundingBox.min.y + depth/2;
	cube.position.z = boundingBox.min.z + height/2;

	transformerUI.add(cube);

        var planeGeometry = new THREE.PlaneGeometry(width, depth); 
	var planeMesh = new THREE.Mesh(planeGeometry, SS.constructors.faceMaterial);
        planeMesh.doubleSided = true;
        planeMesh.position.x = boundingBox.min.x + width/2;
        planeMesh.position.y = boundingBox.min.y + depth/2;
        planeMesh.name = {transformerElement: 'translate-plane'};

        transformerUI.add(planeMesh);
        uiElements.push(planeMesh);

        SS.sceneView.scene.add(transformerUI);
    }

    var activateRotate = function(geomNode) {
        uiElements = [];
        SS.sceneView.scene.remove(transformerUI);
        
        transformerUI = new THREE.Object3D();
        var boundingBox = SS.boundingBoxForGeomNode(geomNode);
        
        var width = boundingBox.max.x - boundingBox.min.x;
        var depth = boundingBox.max.y - boundingBox.min.y;
        var height = boundingBox.max.z - boundingBox.min.z;

        var center = centerOfGeom(boundingBox);

        var jutOut = 10;
        ['x', 'y', 'z'].map(function(key) {
            var lineGeom = new THREE.Geometry();
            var from = center.clone(), to = center.clone();
            from[key] = boundingBox.min[key] - jutOut;
            to[key]   = boundingBox.max[key] + jutOut;

	    lineGeom.vertices.push(new THREE.Vertex(from), new THREE.Vertex(to));
	    var line = new THREE.Line(lineGeom, SS.constructors.lineMaterial);
	    transformerUI.add(line);
        });


        SS.sceneView.scene.add(transformerUI);
    }
    
    var activate = function(geomNode) {
        if (uiState === UISTATE.SCALE) {
            activateRotate(geomNode);
            uiState = UISTATE.ROTATE;
        } else {
            activateScale(geomNode);
            uiState = UISTATE.SCALE;
        }
    }

    var deactivate = function() {
        uiElements = [];
        SS.sceneView.scene.remove(transformerUI);
        uiState = UISTATE.DEACTIVATED;
    }

    SS.sceneView.workplane.on('workplaneXYCursorUpdated', function(event) {
        lastWorkplanePosition = {x: event.x, y: event.y};
        if (translatingState) {
            var translateParams = translatingState.transform.parameters;
            translateParams.u = event.x - translatingState.from.x;
            translateParams.v = event.y - translatingState.from.y;

            transformerUI.position.x = translateParams.u;
            transformerUI.position.y = translateParams.v;
            transformerUI.position.z = translateParams.w;

            SS.moveGeomNodeRendering(translatingState.originalNode, transformerUI.position);
        }

        if (uiState === UISTATE.SCALE) {
            var found = SS.selectInScene(SS.sceneView.scene, SS.sceneView.camera, event.originalEvent);
            var foundTransformerUIElements = found.filter(function(obj) {
                return obj.object.name.transformerElement;
            });
            
            var elementToHighlight = undefined;
            if (foundTransformerUIElements.length > 0) {
                elementToHighlight = foundTransformerUIElements[0].object;
            }

            if (elementToHighlight && (elementToHighlight.name.transformerElement === 'translate-plane')) {
                that.cursor = 'move';
            } else {
                that.cursor = undefined;
            }

            for (var i in uiElements) {
                if (elementToHighlight === uiElements[i]) {
                    elementToHighlight.material.color.setHex(0xffffff);
                    elementToHighlight.material.opacity = 1.0;
                } else {
                    uiElements[i].material.color.setHex(SS.constructors.faceColor);
                }
            }
            
        }
        
    });

    SS.sceneView.workplane.on('workplaneZCursorUpdated', function(event) {
        if (translatingState) {
        }
    });

    SS.sceneView.workplane.on('workplaneClicked', function(event) {
        if (translatingState) {
            var cmd = update_geom_command(translatingState.originalNode, 
                                          translatingState.originalNode, 
                                          translatingState.editingNode);
            command_stack.execute(cmd);
            translatingState = undefined;
        }
    });

    this.selectionUpdated = function(event) {
        if (event.deselected) {
            deactivate();
        } else if (event.selected) {
            if (selectionManager.size() === 1) {
                activate(geom_doc.findById(selectionManager.getSelected()[0]));
            } else {
                deactivate();
            }
        }
    }

    this.initiateTranslate = function(event, geomNode) {
        console.log('translate started');

        var boundingBox = SS.boundingBoxForGeomNode(geomNode);
        var center = centerOfGeom(boundingBox);
        
        var editingNode = geomNode.editableCopy();
        var transform = new Transform({
            type: 'translate',
            editing: true,
	    origin: {x: center.x, y: center.y, z: center.z},
            parameters: {u: 0, v: 0, w: 0, n:0}
        });

        editingNode.transforms.push(transform);

        translatingState = {from: {x: lastWorkplanePosition.x, y: lastWorkplanePosition.y},
                            originalNode: geomNode,
                            editingNode:  editingNode,
                            transform: transform};
    }

    this.isMouseOverTransformerElement = function(scene, camera, event) {

        var found = SS.selectInScene(scene, camera, event);
        var foundTransformerUIElements = found.filter(function(obj) {
            return obj.object.name.transformerElement;
        });

        if (foundTransformerUIElements.length > 0) {
            var element = foundTransformerUIElements[0].object.name.transformerElement;
            if (element = 'translate-plane') {
                SS.transformerManager.initiateTranslate(
                    event,
                    geom_doc.findById(selectionManager.getSelected()[0]));
            }
        }
        
        return foundTransformerUIElements.length > 0;

    }

    
}

