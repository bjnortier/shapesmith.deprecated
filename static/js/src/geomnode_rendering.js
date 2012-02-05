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

    var createGeometry = function(meshes) {
        return {'faces' : create3DGeometries(meshes['faces']),
                'edges' : create1DGeometries(meshes['edges'])};
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


    SS.renderGeometry = function(geomNode) {

        if (!(geom_doc.isRoot(geomNode) && geomNode.mesh)) {
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

        var lineGeometries = create1DGeometries(geomNode.mesh['edges']);
        var lineMaterial = new THREE.LineBasicMaterial({ color: color, opacity: 1.0, linewidth: 2, transparent:true });
        var lines = new THREE.Object3D();
        lineGeometries.map(function(lineGeometry) {
            var line = new THREE.Line(lineGeometry, lineMaterial);
            line.name = objectName;
            lines.add(line);
        });
        lines.name = objectName;

        var selectionGeometriesForLines = create1DSelectionGeometries(geomNode.mesh['edges']);
        var selectionMeshes = new THREE.Object3D();
        selectionGeometriesForLines.map(function(selectionGeometriesForLine) {
            var mesh = new THREE.Mesh(selectionGeometriesForLine, new THREE.MeshBasicMaterial({ color: 0x666666, opacity: 0 })); 
            mesh.name = objectName
            mesh.doubleSided = true;
            selectionMeshes.add(mesh);
        });
        selectionMeshes.name = objectName;                       
        
        SS.sceneView.scene.add(triangles);
        SS.sceneView.scene.add(lines);
        SS.sceneView.scene.add(selectionMeshes);

        geomNode.sceneObjects = {'faces': triangles, 
                                 'edges': lines,
                                 'selectionForEdges' : selectionMeshes};
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
        SS.setOthersTransparent(geomNode);
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

  
    SS.setOthersTransparent = function(geomNode) {
        geom_doc.rootNodes.map(function(rootNode) {
            if (geomNode.id !== rootNode.id) {
                for (key in rootNode.sceneObjects) {
                    rootNode.sceneObjects[key].children.map(function(child) {
                        if (child.material.opacity === 1.0) {
                            child.material.opacity = 0.5;
                        }
                    });
                }
            }
         });
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

    SS.moveGeomNodeRendering = function(geomNode, position) {
        if (geomNode.sceneObjects) {
            for (key in geomNode.sceneObjects) {
                geomNode.sceneObjects[key].position.x = position.x;
                geomNode.sceneObjects[key].position.y = position.y;
                geomNode.sceneObjects[key].position.z = position.z;
            }
        }
    }

    SS.snapshotGeometry = function(geomNode) {
        var snapshot = {};
        if (geomNode.sceneObjects) {
            for (key in geomNode.sceneObjects) {
                geomNode.sceneObjects[key].children.map(function(child) {
                    var geometry = child.geometry;
                    snapshot[key] = snapshot[key] || {};
                    snapshot[key][child] = geometry.vertices;
                });
            }
        }
        return snapshot;
    }

    SS.scaleGeomNodeRendering = function(geomNode, scalePoint, factor, snapshot) {
        if (geomNode.sceneObjects) {
            for (key in geomNode.sceneObjects) {
                geomNode.sceneObjects[key].children.map(function(child) {

                    var geometry = child.geometry;
                    var snapshotVertices = snapshot[key][child];
                    geometry.vertices = snapshotVertices.map(function(vertex) {
	                var position = vertex.position.clone();
	                position.x = scalePoint.x + (position.x - scalePoint.x)*factor;
	                position.y = scalePoint.y + (position.y - scalePoint.y)*factor;
	                position.z = scalePoint.z + (position.z - scalePoint.z)*factor;
	                return new THREE.Vertex(position);
	            });

                    geometry.__dirtyVertices = true;
                });
            }
        }
    }

    SS.boundingBoxForGeomNode = function(geomNode) {
        var boundingBoxes = [];
        var addFunction = function(child) {
            //if (!child.geometry.boundingBox) {
                child.geometry.computeBoundingBox();
            //}
            boundingBoxes.push(child.geometry.boundingBox);
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
        
        /*for (key in min) {
            if (typeof(min[key]) === 'number') {
                if (min[key] < 0) {
                    min[key] = -Math.ceil(-min[key]);
                } else {
                    min[key] = Math.floor(min[key]);
                }
            }
        }
        
        for (key in max) {
            if (typeof(max[key]) === 'number') {
                if (max[key] < 0) {
                    max[key] = -Math.floor(-max[key]);
                } else {
                    max[key] = Math.ceil(max[key]);
                }
            }
        }*/
        
        return {min: min, max: max};
    }

})();