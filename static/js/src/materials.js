var SS = SS || {};
SS.materials = {};

SS.materials.lineColor = 0x66A1D2;
SS.materials.faceColor = 0x0b5fa5;
SS.materials.faceMaterial = new THREE.MeshBasicMaterial( { color: SS.materials.faceColor, transparent: true, opacity: 0.2 } );
SS.materials.solidFaceMaterial = new THREE.MeshBasicMaterial( { color: SS.materials.faceColor } );
SS.materials.lineMaterial = new THREE.LineBasicMaterial({ color: SS.materials.lineColor, wireframe : true, linewidth: 1.0 });
SS.materials.wireframeMaterial = new THREE.MeshBasicMaterial( { color: SS.materials.lineColor, wireframe: true } )

SS.materials.anchorGeometry = new THREE.CubeGeometry(1.0, 1.0, 1.0);
SS.materials.anchorMaterial = new THREE.MeshBasicMaterial( { color: 0x66a1d1, opacity: 0.8, wireframe: false } );

