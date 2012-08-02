var SS = SS || {};

SS.colors = {};
SS.colors.white = 0xffffff;
SS.colors.red   = 0xff0000;
SS.colors.green = 0x00ff00;
SS.colors.blue  = 0x0000ff;

SS.colors.xAxis = SS.colors.blue;
SS.colors.yAxis = SS.colors.green;
SS.colors.zAxis = SS.colors.red;
SS.colors.fadingGrid = 0x66A1D2;

SS.materials = {};

SS.materials.lineColor = 0x66A1D2;
SS.materials.faceColor = 0x0b5fa5;
SS.materials.faceMaterial = new THREE.MeshBasicMaterial( { color: SS.materials.faceColor, transparent: true, opacity: 0.2 } );
SS.materials.solidFaceMaterial = new THREE.MeshBasicMaterial( { color: SS.materials.faceColor } );
SS.materials.lineMaterial = new THREE.LineBasicMaterial({ color: SS.materials.lineColor, wireframe : true, linewidth: 1.0 });
SS.materials.wireframeMaterial = new THREE.MeshBasicMaterial( { color: SS.materials.lineColor, wireframe: true } )

SS.materials.anchorGeometry = new THREE.CubeGeometry(1.0, 1.0, 1.0);
SS.materials.anchorMaterial = new THREE.MeshBasicMaterial( { color: 0x66a1d1, opacity: 0.8, wireframe: false } );

SS.materials.constructionLineMaterial = new THREE.LineBasicMaterial({ color: 0x777777, wireframe : true, linewidth: 1.0 });

SS.materials.gridMajor = new THREE.LineBasicMaterial({ color: 0x66A1D2, opacity: 0.2, transparent: true });
SS.materials.gridMinor = new THREE.LineBasicMaterial({ color: 0x66A1D2, opacity: 0.02, transparent: true });
SS.materials.globalXYPlane = [new THREE.MeshBasicMaterial( { color: 0x333333, wireframe: true })];



