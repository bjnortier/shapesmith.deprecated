var SS = SS || {};
SS.transformers = {};

SS.transformers.centerOfGeom = function(boundingBox) {
    return new THREE.Vector3().add(boundingBox.min, 
                                   new THREE.Vector3().sub(boundingBox.max, boundingBox.min).divideScalar(2));
}

SS.transformers.TranslateElement = function(geomNode) {
    
    var boundingBox = SS.boundingBoxForGeomNode(geomNode);
    var width = boundingBox.max.x - boundingBox.min.x;
    var depth = boundingBox.max.y - boundingBox.min.y;
    var height = boundingBox.max.z - boundingBox.min.z;

    var planeGeometry = new THREE.PlaneGeometry(width, depth); 
    var planeMesh = new THREE.Mesh(planeGeometry, 
                                   new THREE.MeshBasicMaterial({color: SS.constructors.faceColor, 
                                                                transparent: true, 
                                                                opacity: 0.5}));

    planeMesh.doubleSided = true;
    planeMesh.position.x = boundingBox.min.x + width/2;
    planeMesh.position.y = boundingBox.min.y + depth/2;
    planeMesh.position.z = -0.05;
    planeMesh.name = {transformerElement: 'translate-XY'};
    
    this.sceneObject = planeMesh;

    this.highlight = function() {
        this.sceneObject.material.opacity = 1.0;
    }

    this.unHighlight = function() {
        this.sceneObject.material.opacity = 0.5;
    }

    this.cursor = 'move';
}

SS.transformers.ScaleElement = function(geomNode, elementName) {
    
    var boundingBox = SS.boundingBoxForGeomNode(geomNode);

    var arrowGeometry = new THREE.Geometry();
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(2, -1.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(2, -0.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(3, -0.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(3, -1.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(5, 0, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(3, 1.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(3, 0.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(2, 0.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(2, 1.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
    
    arrowGeometry.faces.push(new THREE.Face4(2,3,7,8));
    arrowGeometry.faces.push(new THREE.Face3(0,1,9));
    arrowGeometry.faces.push(new THREE.Face3(4,5,6));
    arrowGeometry.computeCentroids();
    arrowGeometry.computeFaceNormals();

    var arrowMesh = new THREE.Mesh(arrowGeometry, 
                                   new THREE.MeshBasicMaterial({color: SS.constructors.faceColor, 
                                                                transparent: true, 
                                                                opacity: 0.5}));
    arrowMesh.name = {transformerElement: elementName};
    
    var lineGeom = new THREE.Geometry();
    lineGeom.vertices = arrowGeometry.vertices;
    var line = new THREE.Line(lineGeom, 
                              new THREE.LineBasicMaterial({color: SS.constructors.lineColor, 
                                                           wireframe : true, 
                                                           linewidth: 2.0, 
                                                           transparent: true, 
                                                           opacity: 0.5 }));
    line.name = {transformerElement: elementName};
    
    this.sceneObject = new THREE.Object3D();
    this.sceneObject.add(arrowMesh);
    this.sceneObject.add(line);
    
    

    this.highlight = function() {
        this.sceneObject.children.map(function(child) {
            child.material.opacity = 1.0;
        });
    }

    this.unHighlight = function() {
        this.sceneObject.children.map(function(child) {
            child.material.opacity = 0.5;
        });
    }

    this.cursor = 'move';
}

SS.transformers.Manager = function() {

    var that = this;
    var transformerUI = new THREE.Object3D();
    var UISTATE = {DEACTIVATED: 0, SCALE: 1, ROTATE: 2};
    var uiState = UISTATE.DEACTIVATED;
    var transformingState = undefined;
    var lastWorkplanePosition = undefined;

    var cube;
    var uiElements = [];

    var addBoundingBox = function(geomNode) {
        if (cube) {
            transformerUI.remove(cube);
        }
        
        var boundingBox = SS.boundingBoxForGeomNode(geomNode);
        var center = SS.transformers.centerOfGeom(boundingBox);

        var width = boundingBox.max.x - boundingBox.min.x;
        var depth = boundingBox.max.y - boundingBox.min.y;
        var height = boundingBox.max.z - boundingBox.min.z;

        var geometry = new THREE.CubeGeometry(width, depth, height);
	cube = new THREE.Mesh(geometry, new THREE.MeshBasicMaterial({color: SS.constructors.lineColor, 
                                                                     wireframe: true}));
        
	cube.position.x = boundingBox.min.x + width/2;
	cube.position.y = boundingBox.min.y + depth/2;
	cube.position.z = boundingBox.min.z + height/2;
	transformerUI.add(cube);
    }

    var activateScale = function(geomNode) {
        uiElements = [];
        SS.sceneView.scene.remove(transformerUI);

        transformerUI = new THREE.Object3D();
        addBoundingBox(geomNode);
        var boundingBox = SS.boundingBoxForGeomNode(geomNode);

        var translateElement = new SS.transformers.TranslateElement(geomNode);
        uiElements.push(translateElement);
        transformerUI.add(translateElement.sceneObject);

        var scaleElementSpec = {'scale1' : {x: boundingBox.max.x + 1,
                                           y: boundingBox.max.y + 1,
                                           zRotation: 1/4*Math.PI},
                                'scale2' : {x: boundingBox.min.x - 1,
                                           y: boundingBox.max.y + 1,
                                           zRotation: 3/4*Math.PI},
                                'scale3' : {x: boundingBox.min.x - 1,
                                           y: boundingBox.min.y - 1,
                                           zRotation: 5/4*Math.PI},
                                'scale4' : {x: boundingBox.max.x + 1,
                                           y: boundingBox.min.y - 1,
                                           zRotation: 7/4*Math.PI}};
                                
        for (name in scaleElementSpec) {
            var element = new SS.transformers.ScaleElement(geomNode, name);
            uiElements.push(element);
            element.sceneObject.position.x = scaleElementSpec[name].x;
            element.sceneObject.position.y = scaleElementSpec[name].y;
            element.sceneObject.rotation.z = scaleElementSpec[name].zRotation;
            transformerUI.add(element.sceneObject);
        }

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

        var center = SS.transformers.centerOfGeom(boundingBox);

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

    var highlightElement = function(event) {
        var found = SS.selectInScene(SS.sceneView.scene, SS.sceneView.camera, event.originalEvent);
        var foundUIElement;
        for (var i = 0; (i < found.length) && !foundUIElement; ++i) {
            for (var j = 0; (j < uiElements.length) && !foundUIElement; ++j) {
                if ((found[i].object === uiElements[j].sceneObject) || 
                    (found[i].object.parent && (found[i].object.parent === uiElements[j].sceneObject))) {
                    foundUIElement = uiElements[j];
                }
            }
        }

        uiElements.map(function(element) {
            if (element !== foundUIElement) {
                element.unHighlight();
            }
        });
        if (foundUIElement) {
            foundUIElement.highlight();
            that.cursor = foundUIElement.cursor;
        } else {
            that.cursor = undefined;
        }
    }

    SS.sceneView.workplane.on('workplaneXYCursorUpdated', function(event) {
        lastWorkplanePosition = {x: event.x, y: event.y};
        
        if (transformingState) {

            if (transformingState.transform.type === 'translate') {
                transformingState.transform.parameters.u = event.x - transformingState.from.x
                transformingState.transform.parameters.v = event.y - transformingState.from.y

                transformerUI.position.x = transformingState.transform.parameters.u;
                transformerUI.position.y = transformingState.transform.parameters.v;
                transformerUI.position.z = transformingState.transform.parameters.w;
                
                SS.moveGeomNodeRendering(transformingState.originalNode, transformerUI.position);
            } else if (transformingState.transform.type === 'scale') {
                var dxFrom =  transformingState.from.x - transformingState.center.x;
                var dyFrom =  transformingState.from.y - transformingState.center.y;
                var r1 = Math.sqrt(dxFrom*dxFrom + dyFrom*dyFrom);


                var dxTo = event.x - transformingState.center.x;
                var dyTo = event.y - transformingState.center.y;
                var r2 = Math.sqrt(dxTo*dxTo + dyTo*dyTo);

                console.log('from:' + JSON.stringify(transformingState.from));
                console.log('center:' + JSON.stringify(transformingState.center));
                console.log('event:' + JSON.stringify({x: event.x, y:event.y}));


                var factor = r2/r1;
                
                var scalePoint = transformingState.center.clone();
                scalePoint.z = 0;

                SS.scaleGeomNodeRendering(transformingState.originalNode, 
                                          scalePoint, 
                                          factor, 
                                          transformingState.originalGeometrySnapshot);

                transformingState.transform.parameters.factor = factor;
                activateScale(transformingState.originalNode);

            }
        }
        highlightElement(event);

    });

    SS.sceneView.workplane.on('workplaneZCursorUpdated', function(event) {
    });

    SS.sceneView.workplane.on('workplaneClicked', function(event) {
        if (transformingState) {
            var doTranslate = 
                ((transformingState.transform.type === 'translate') &&
                 ((transformingState.transform.parameters.u !== 0) ||
                  (transformingState.transform.parameters.v !== 0) ||
                  (transformingState.transform.parameters.w !== 0)));
            var doScale = ((transformingState.transform.type === 'scale') &&
                           (transformingState.transform.parameters.factor !== 1.0));

            if (doTranslate || doScale) {
                var cmd = update_geom_command(transformingState.originalNode, 
                                              transformingState.originalNode, 
                                              transformingState.editingNode);
                command_stack.execute(cmd);
            }
        }
        transformingState = undefined;
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

    var initiateTransform = function(event, geomNode, type, parameters) {
        var boundingBox = SS.boundingBoxForGeomNode(geomNode);
        var center = SS.transformers.centerOfGeom(boundingBox);
        
        var editingNode = geomNode.editableCopy();
        var transform = new Transform({
            type: type,
            editing: true,
	    origin: {x: center.x, y: center.y, z: 0},
            parameters: parameters
        });

        editingNode.transforms.push(transform);
        transformingState = {from: {x: lastWorkplanePosition.x, y: lastWorkplanePosition.y},
                             center: center,
                             originalNode: geomNode,
                             editingNode:  editingNode,
                             transform: transform};
    }

    this.initiateTranslate = function(event, geomNode) {
        initiateTransform(event, geomNode, 'translate', {u: 0, v: 0, w: 0, n:0});
    }

    this.initiateScale = function(event, geomNode) {
        initiateTransform(event, geomNode, 'scale', {factor: 1.0});
        transformingState.originalGeometrySnapshot = SS.snapshotGeometry(geomNode);
    }

    this.isMouseOverTransformerElement = function(scene, camera, event) {

        var found = SS.selectInScene(scene, camera, event);
        var foundTransformerUIElements = found.filter(function(obj) {
            return obj.object.name.transformerElement;
        });
   
        return foundTransformerUIElements.length > 0;
    }

    this.initiateTransformer = function(scene, camera, event) {

        var found = SS.selectInScene(scene, camera, event);
        var foundTransformerUIElements = found.filter(function(obj) {
            return obj.object.name.transformerElement;
        });

        if (foundTransformerUIElements.length > 0) {
            var element = foundTransformerUIElements[0].object.name.transformerElement;
            var geomNode = geom_doc.findById(selectionManager.getSelected()[0]);
            if (element === 'translate-XY') {
                SS.transformerManager.initiateTranslate(event, geomNode);
            }
            if (element.match(/scale.*/)) {
                SS.transformerManager.initiateScale(event, geomNode);
            }
        }
        
        return foundTransformerUIElements.length > 0;

    }

    
}

