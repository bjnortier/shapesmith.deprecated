var SS = SS || {};

(function() {

    var unselectedColor = 0x00dd00, selectedColor = 0xdddd00;

    var create1DGeometries = function(mesh) {

       var precisionPoints = 4; 
       var precision = Math.pow( 10, precisionPoints );
       var keyForVertex = function(vertex) {
           var position = vertex.position;
           return [ Math.round( position.x * precision ), Math.round( position.y * precision ), Math.round( position.z * precision ) ].join( '_' );
       }

       var segmentsByStartPosition = {};
       var segmentsByEndPosition = {};
       var allSegments = [];

       mesh.segments.map(function(segment) {

           var vertices = [];
           for (var i = segment.start; i < segment.end; i += 3) {
               var position = new THREE.Vector3(mesh.positions[i], 
					        mesh.positions[i + 1], 
					        mesh.positions[i + 2]);
	       var vertex = new THREE.Vertex(position);
               vertices.push(vertex);
           }
           
           var startVertex = vertices[0];
           var endVertex = vertices[vertices.length-1];
           var startVertexKey = keyForVertex(startVertex);
           var endVertexKey = keyForVertex(endVertex);
           
           allSegments.push(vertices);
           segmentsByStartPosition[startVertexKey] = segmentsByStartPosition[startVertexKey] || [];
           segmentsByStartPosition[startVertexKey].push(vertices);

           segmentsByEndPosition[endVertexKey] = segmentsByEndPosition[endVertexKey] || [];
           segmentsByEndPosition[endVertexKey].push(vertices);
       });

       // Compression
       do {

           var didACompression = false;
           for (vertexKey in segmentsByStartPosition)  {

               if (!didACompression) {
                   // If the is a segment with the same end position key as this 
                   // start position, compress the segments

                   if ((segmentsByStartPosition[vertexKey].length > 0) &&
                       segmentsByEndPosition[vertexKey] &&
                       (segmentsByEndPosition[vertexKey].length > 0)) {

                       var firstSegment = segmentsByEndPosition[vertexKey][0];
                       var secondSegment = segmentsByStartPosition[vertexKey][0];

                       // Circular segments
                       if (!(firstSegment === secondSegment)) {

                           var newSegment = [].concat(firstSegment).concat(secondSegment);

                           newSegmentStartKey = keyForVertex(newSegment[0]);
                           newSegmentEndKey   = keyForVertex(newSegment[newSegment.length - 1]);
                           
                           allSegments.splice(allSegments.indexOf(firstSegment), 1);
                           allSegments.splice(allSegments.indexOf(secondSegment), 1);
                           allSegments.push(newSegment);

                           segmentsByStartPosition[vertexKey].splice(
                               segmentsByStartPosition[vertexKey].indexOf(secondSegment), 1);
                           segmentsByStartPosition[newSegmentStartKey].splice(
                               segmentsByStartPosition[newSegmentStartKey].indexOf(firstSegment), 1);
                           
                           segmentsByEndPosition[vertexKey].splice(
                               segmentsByEndPosition[vertexKey].indexOf(firstSegment), 1);
                           segmentsByEndPosition[newSegmentEndKey].splice(
                               segmentsByEndPosition[newSegmentEndKey].indexOf(secondSegment), 1);
                           

                           segmentsByStartPosition[newSegmentStartKey].push(newSegment);
                           segmentsByEndPosition[newSegmentEndKey].push(newSegment);

                           didACompression = true;
                       }
                   }
               }
               
           }
               
           
       } while(didACompression);

       return allSegments.map(function(segment) {
           var geometry =  new THREE.Geometry();
           geometry.vertices = segment;
           geometry.dynamic = true;
           return geometry;
       });

    }

    var createGeometry = function(meshes) {
        return {'faces' : create3DGeometries(meshes['faces']),
                'edges' : create1DGeometries(meshes['edges'])};
    }

    var create1DSelectionGeometries = function(mesh) {
        return mesh.segments.map(function(segment) {
           var positions = [];
            for (var i = segment.start; i < segment.end; i+=3) {
                positions.push(new THREE.Vector3(mesh.positions[i], 
					         mesh.positions[i + 1], 
					         mesh.positions[i + 2]));
            }
            return new THREE.PipeGeometry(3, positions);
        });
    }

    var createVertexGeometries = function(mesh) {
        var precisionPoints = 4; 
        var precision = Math.pow( 10, precisionPoints );
        var keyForPosition = function(position) {
            return [ Math.round( position.x * precision ), Math.round( position.y * precision ), Math.round( position.z * precision ) ].join( '_' );
        }

        var positions = {};
        var vertexMaterial = new THREE.MeshBasicMaterial({color: 0xffffff, opacity: 0.8});
        mesh.segments.map(function(segment) {
            var startPosition = new THREE.Vector3(mesh.positions[segment.start], 
					          mesh.positions[segment.start + 1], 
					          mesh.positions[segment.start + 2]);
            var endPosition = new THREE.Vector3(mesh.positions[segment.end - 2], 
					      mesh.positions[segment.end - 1], 
					      mesh.positions[segment.end]);
            [startPosition, endPosition].map(function(position) {
                var key = keyForPosition(position);
                if (!positions[key]) {
                    positions[key] = new THREE.Mesh(new THREE.CubeGeometry(0.5, 0.5, 0.5), vertexMaterial);
                    positions[key].position = position;
                }
            });
        });

        var cubes = [];
        for (var key in positions) {
            cubes.push(positions[key]);
        }
        return cubes;
    }

    var create3DGeometries = function(mesh) {
	var geometry = new THREE.Geometry();

	for (var i = 0; i  < mesh.positions.length/3; ++i) {
	    var position = new THREE.Vector3(mesh.positions[i * 3], 
					     mesh.positions[i * 3 + 1], 
					     mesh.positions[i * 3 + 2]);
	    var vertex = new THREE.Vertex(position);
	    geometry.vertices.push(vertex);
	}
	
	for (var i = 0; i < mesh.indices.length/3; ++i) {
	    var a = mesh.indices[i * 3],
	    b = mesh.indices[i * 3 + 1],
	    c = mesh.indices[i * 3 + 2];

	    var face = new THREE.Face3(a,b,c);
	    face.vertexNormals = [new THREE.Vector3(mesh.normals[a*3], 
						    mesh.normals[a*3+1], 
						    mesh.normals[a*3+2]),
				  new THREE.Vector3(mesh.normals[b*3], 
						    mesh.normals[b*3+1], mesh.normals[b*3+2]),
				  new THREE.Vector3(mesh.normals[c*3], 
						    mesh.normals[c*3+1], mesh.normals[c*3+2])];
	    geometry.faces.push(face);
	}
	geometry.computeCentroids();
	geometry.computeFaceNormals();
        geometry.dynamic = true;
	return [geometry];
    }

    SS.createGeometry = function(geomNode) {

        if (!geomNode.mesh) {
            return;
        }
        
        var color = unselectedColor, opacity = 1.0;
        var isEditing = geomNode.editing;
        for (index in geomNode.transforms) {
	    if (geomNode.transforms[index].editing) {
	        isEditing = true;
	    }
        }
        if (isEditing) {
	    color = 0xdddd00;
	    opacity = 0.5;
        }
        var objectName = {geomNodeId: geomNode.id};

        var triangleGeometries = create3DGeometries(geomNode.mesh['faces']);
        var triangleMaterial = new THREE.MeshPhongMaterial( { ambient: color, color: color, opacity: opacity,  specular: color, shininess: 50, shading: THREE.SmoothShading, transparent:true } );
        var triangles = new THREE.Object3D();
        triangleGeometries.map(function(triangleGeometry) {
            var mesh = new THREE.Mesh(triangleGeometry, triangleMaterial);
            mesh.doubleSided = true; 
            mesh.name = objectName
            triangles.add(mesh);
        });
        triangles.name = objectName;

        //var vertexGeometries = createVertexGeometries(geomNode.mesh['edges']);
        // var vertices = new THREE.Object3D();
        // vertexGeometries.map(function(vertex) {
        //     vertices.add(vertex);
        // });
        //vertices.names = objectName;

        var lineGeometries = create1DGeometries(geomNode.mesh['edges']);
        var lineMaterial = new THREE.LineBasicMaterial({ color: color, opacity: 1.0, linewidth: 2, transparent:true });
        var lines = new THREE.Object3D();
        lineGeometries.map(function(lineGeometry) {
            var line = new THREE.Line(lineGeometry, lineMaterial);
            line.name = objectName;
            lines.add(line);
        });
        lines.name = objectName;

        // Only render line selections when there are no triangles
        var selectionMeshes = new THREE.Object3D();
        if (triangles.children.length > 0) {
            var selectionGeometriesForLines = create1DSelectionGeometries(geomNode.mesh['edges']);
            selectionGeometriesForLines.map(function(selectionGeometriesForLine) {
                var mesh = new THREE.Mesh(selectionGeometriesForLine, new THREE.MeshBasicMaterial({ color: 0x666666, opacity: 0 })); 
                mesh.name = objectName
                mesh.doubleSided = true;
                selectionMeshes.add(mesh);
            });
            selectionMeshes.name = objectName; 
        }                      
        
        return {'faces': triangles, 
                //'vertices' : vertices,
                'edges': lines,
                'selectionForEdges' : selectionMeshes};
    }
    
    SS.renderGeometry = function(geomNode) {
        var geometries = SS.createGeometry(geomNode);
        if (geometries) {
            SS.sceneView.scene.add(geometries.faces);
            SS.sceneView.scene.add(geometries.edges);
            SS.sceneView.scene.add(geometries.selectionForEdges);
            geomNode.sceneObjects = geometries;
        }
    }

    SS.hideGeometry = function(geomNode) {
        if (geomNode.sceneObjects) {
            for (key in geomNode.sceneObjects) {
	        scene.remove(geomNode.sceneObjects[key]);
            }
            geomNode.sceneObjects = {};
        }
    }

    SS.highlightGeometry = function(geomNode) {
        for (key in geomNode.sceneObjects) {
            var object = geomNode.sceneObjects[key];
            SS.sceneView.scene.remove(object);
            SS.sceneView.scene.add(object);
        };
        geomNode.sceneObjects['faces'].children.map(function(child) {
            child.material.color.setHex(selectedColor);
	    child.material.ambient.setHex(selectedColor);
	    child.material.specular.setHex(selectedColor);
            child.material.opacity = 0.7;
        });
        geomNode.sceneObjects['edges'].children.map(function(child) {
            child.material.color.setHex(selectedColor);
            child.material.opacity = 0.7;
        });
    }

    SS.unhighlightGeometry = function(geomNode) {
        SS.restoreOpacity();
        geomNode.sceneObjects['faces'].children.map(function(child) {
            child.material.color.setHex(unselectedColor);
	    child.material.ambient.setHex(unselectedColor);
	    child.material.specular.setHex(unselectedColor);
            child.material.opacity = 1.0;
        });
        geomNode.sceneObjects['edges'].children.map(function(child) {
            child.material.color.setHex(unselectedColor);
            child.material.opacity = 1.0;
        });
    }

  
    SS.setTransparent = function(geomNode) {
        for (key in geomNode.sceneObjects) {
            geomNode.sceneObjects[key].children.map(function(child) {
                if (child.material.opacity === 1.0) {
                    child.material.opacity = 0.5;
                }
            });
        }
    }
    
    SS.restoreOpacity = function() {
        geom_doc.rootNodes.map(function(geomNode) {
            if (geomNode.sceneObjects) {
                for (key in geomNode.sceneObjects) {
                    geomNode.sceneObjects[key].children.map(function(child) {
                        if ((key !== 'selectionForEdges') && 
                            (child.material.opacity !== 1.0)) {
                            child.material.opacity = 1.0;
                        }
                    });
                }
            }
        });
    }

    SS.translateGeomNodeRendering = function(originalNode, editingNode,  translation) {
        for (key in editingNode.sceneObjects) {
            for (var i = 0; i < editingNode.sceneObjects[key].children.length; ++i) {

                var originalGeometry = originalNode.originalSceneObjects[key].children[i].geometry;
                var editingGeometry = editingNode.sceneObjects[key].children[i].geometry;

                editingGeometry.vertices = originalGeometry.vertices.map(function(vertex) {
	            var position = vertex.position.clone();
	            position.x = position.x + translation.x;
	            position.y = position.y + translation.y;
	            position.z = position.z + translation.z;
	            return new THREE.Vertex(position);
	        });

                editingGeometry.computeCentroids();
	        editingGeometry.computeFaceNormals();
                editingGeometry.__dirtyVertices = true;
            }
        }
    }

    SS.scaleGeomNodeRendering = function(originalNode, editingNode, scalePoint, factor) {
        for (key in editingNode.sceneObjects) {
            for (var i = 0; i < editingNode.sceneObjects[key].children.length; ++i) {

                var originalGeometry = originalNode.originalSceneObjects[key].children[i].geometry;
                var editingGeometry = editingNode.sceneObjects[key].children[i].geometry;

                editingGeometry.vertices = originalGeometry.vertices.map(function(vertex) {
	            var position = vertex.position.clone();
	            position.x = scalePoint.x + (position.x - scalePoint.x)*factor;
	            position.y = scalePoint.y + (position.y - scalePoint.y)*factor;
	            position.z = scalePoint.z + (position.z - scalePoint.z)*factor;
	            return new THREE.Vertex(position);
	        });

                editingGeometry.computeCentroids();
	        editingGeometry.computeFaceNormals();
                editingGeometry.__dirtyVertices = true;
            }
        }
    }

    SS.rotateGeomNodeRendering = function(originalNode, editingNode, center, axis, angle) {
        var normUVW = axis.clone().normalize();
        var un = normUVW.x, vn = normUVW.y, wn = normUVW.z;

        for (key in editingNode.sceneObjects) {

            for (var i = 0; i < editingNode.sceneObjects[key].children.length; ++i) {
                
                var originalGeometry = originalNode.originalSceneObjects[key].children[i].geometry;
                var editingGeometry = editingNode.sceneObjects[key].children[i].geometry;

	        editingGeometry.vertices = originalGeometry.vertices.map(function(vertex) {
	            var position = vertex.position.clone();
                    position.x = position.x - center.x;
                    position.y = position.y - center.y;
                    position.z = position.z - center.z;
                    
                    // http://blog.client9.com/2007/09/rotating-point-around-vector.html
                    var a = (un*position.x + vn*position.y + wn*position.z);
                    var x2 = a*un + (position.x - a*un)*Math.cos(angle/180*Math.PI) 
                        + (vn*position.z - wn*position.y)*Math.sin(angle/180*Math.PI);
                    var y2 = a*vn + (position.y - a*vn)*Math.cos(angle/180*Math.PI) 
                        + (wn*position.x - un*position.z)*Math.sin(angle/180*Math.PI);
                    var z2 = a*wn + (position.z - a*wn)*Math.cos(angle/180*Math.PI) 
                        + (un*position.y - vn*position.x)*Math.sin(angle/180*Math.PI);
                    
	            var newPosition = new THREE.Vector3(center.x + x2, 
                                                        center.y + y2, 
                                                        center.z + z2);
	            return new THREE.Vertex(newPosition);
	        });
                editingGeometry.computeCentroids();
	        editingGeometry.computeFaceNormals();
                editingGeometry.__dirtyVertices = true;
            }
        }
    }

    SS.axisMirrorGeomNodeRendering = function(originalNode, editingNode, transform) {

        var origin = transform.origin;
        var axis = transform.parameters;
        var u = axis.u, v = axis.v, w = axis.w;
        var un, vn, wn;
        var normUVW = new THREE.Vector3(u, v, w).normalize();
        un = normUVW.x;
        vn = normUVW.y;
        wn = normUVW.z;
        
        for (key in editingNode.sceneObjects) {

            for (var i = 0; i < editingNode.sceneObjects[key].children.length; ++i) {
                
                var originalGeometry = originalNode.originalSceneObjects[key].children[i].geometry;
                var editingGeometry = editingNode.sceneObjects[key].children[i].geometry;

	        editingGeometry.vertices = originalGeometry.vertices.map(function(vertex) {
	            var position = vertex.position.clone();
                    
                    position.x = position.x - origin.x;
                    position.y = position.y - origin.y;
                    position.z = position.z - origin.z;
                    
                    var a = (un*position.x + vn*position.y + wn*position.z);
                    var x2 = 2*a*un - position.x + origin.x;
                    var y2 = 2*a*vn - position.y + origin.y;
                    var z2 = 2*a*wn - position.z + origin.z;
                    
	            return new THREE.Vertex(new THREE.Vector3(x2, y2, z2));
                });
                editingGeometry.computeCentroids();
	        editingGeometry.computeFaceNormals();
                editingGeometry.__dirtyVertices = true;
            }
        }            
    }

    SS.planeMirrorGeomNodeRendering = function(originalNode, editingNode, transform) {

        var origin =  new THREE.Vector3(transform.origin.x, 
                                        transform.origin.y, 
                                        transform.origin.z); 
        var normal = new THREE.Vector3(transform.parameters.u, 
                                       transform.parameters.v, 
                                       transform.parameters.w)
            .normalize()
            .negate();
        
        for (key in editingNode.sceneObjects) {

            for (var i = 0; i < editingNode.sceneObjects[key].children.length; ++i) {
                
                var originalGeometry = originalNode.originalSceneObjects[key].children[i].geometry;
                var editingGeometry = editingNode.sceneObjects[key].children[i].geometry;

	        editingGeometry.vertices = originalGeometry.vertices.map(function(vertex) {
	            var position = vertex.position.clone();
                    var dot = new THREE.Vector3().sub(position, origin).dot(normal);
                    var dPos = normal.clone().multiplyScalar(-2*dot);
                    var newPosition = new THREE.Vector3().add(position, dPos);
	            return new THREE.Vertex(newPosition);
                });
                editingGeometry.computeCentroids();
	        editingGeometry.computeFaceNormals();
                editingGeometry.__dirtyVertices = true;
            }
        }            
    }

    SS.boundingBoxForSceneObject = function(sceneObject) {
        var boundingBoxes = [];
        var addRecursively = function(child) {
            if (child.geometry) {
                child.geometry.computeBoundingBox();
                child.geometry.boundingBox && boundingBoxes.push(child.geometry.boundingBox);
            }
            for (i in child.children) {
                addRecursively(child.children[i]);
            }
        };
        addRecursively(sceneObject);

        if (boundingBoxes.length == 0) {
            return {min: new THREE.Vector3(0,0,0), max: new THREE.Vector3(0,0,0)};
        }

        var min = boundingBoxes[0].min;
        var max = boundingBoxes[0].max;
        for (var i = 1; i < boundingBoxes.length; ++i) {
            var box = boundingBoxes[i];
            min.x = Math.min(min.x, box.min.x);
            min.y = Math.min(min.y, box.min.y);
            min.z = Math.min(min.z, box.min.z);
            max.x = Math.max(max.x, box.max.x);
            max.y = Math.max(max.y, box.max.y);
            max.z = Math.max(max.z, box.max.z);
        }
        
        return {min: min, max: max};
    }

    SS.boundingBoxForGeomNode = function(geomNode) {
        var boundingBoxes = [];
        var addFunction = function(child) {
            child.geometry.computeBoundingBox();
            child.geometry.boundingBox && boundingBoxes.push(child.geometry.boundingBox);
        };
            
        geomNode.sceneObjects.edges.children.map(addFunction);
        geomNode.sceneObjects.faces.children.map(addFunction);

        var min = boundingBoxes[0].min;
        var max = boundingBoxes[0].max;
        for (var i = 1; i < boundingBoxes.length; ++i) {
            var box = boundingBoxes[i];
            min.x = Math.min(min.x, box.min.x);
            min.y = Math.min(min.y, box.min.y);
            min.z = Math.min(min.z, box.min.z);
            max.x = Math.max(max.x, box.max.x);
            max.y = Math.max(max.y, box.max.y);
            max.z = Math.max(max.z, box.max.z);
        }
        
        return {min: min, max: max};
    }
    
    SS.centerOfGeom = function(boundingBox) {
        return new THREE.Vector3().add(boundingBox.min, 
                                       new THREE.Vector3().sub(boundingBox.max, boundingBox.min).divideScalar(2));
    }

})();
