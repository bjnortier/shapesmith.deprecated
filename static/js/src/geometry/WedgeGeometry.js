// Adapted from CubeGeometry.js in THREE.js
// https://github.com/mrdoob/three.js/
//

THREE.WedgeGeometry = function ( width, height, depth, dWidth, materials, flipped, sides ) {

    THREE.Geometry.call( this );

    var scope = this,
    width_half = width / 2,
    height_half = height / 2,
    depth_half = depth / 2,
    flip = flipped ? - 1 : 1;

    if ( materials !== undefined ) {
        if ( materials instanceof Array ) {
            this.materials = materials;
        } else {
            this.materials = [];
            for ( var i = 0; i < 6; i ++ ) {
                this.materials.push( [ materials ] );
            }
        }
    } else {
        this.materials = [];
    }

    this.sides = { px: true, nx: true, py: true, ny: true, pz: true, nz: true };
    if( sides != undefined ) {
        for( var s in sides ) {
            if ( this.sides[ s ] != undefined ) {
                this.sides[ s ] = sides[ s ];
            }
        }
    }

    this.sides.px && buildPlane( 'z', 'y',   1 * flip, - 1, depth, height, - width_half, dWidth, this.materials[ 0 ] ); // px
    this.sides.nx && buildSkewPlane(- 1 * flip, - 1, depth, height, width_half, dWidth, this.materials[ 1 ] );   // nx
    this.sides.py && buildPlane( 'x', 'z',   1 * flip,   1, width, depth, height_half, dWidth, this.materials[ 2 ] );   // py
    this.sides.ny && buildPlane( 'x', 'z',   1 * flip, - 1, width, depth, - height_half, dWidth, this.materials[ 3 ] ); // ny
    this.sides.pz && buildWedgePlane(1 * flip, - 1, width, height, depth_half, dWidth, this.materials[ 4 ] );   // pz
    this.sides.nz && buildWedgePlane(- 1 * flip, - 1, width, height, - depth_half, dWidth, this.materials[ 5 ] ); // nz

    mergeVertices();

    function buildWedgePlane(udir, vdir, width, height, depth, dWidth, material ) {

        var u = 'x', v = 'y', w = 'z';
        var w, ix, iy,

        width_half = width / 2,
        height_half = height / 2,
        offset = scope.vertices.length;

        for( iy = 0; iy < 2; iy++ ) {

            for( ix = 0; ix < 2; ix++ ) {

                var vector = new THREE.Vector3();
                if (udir == -1 && ix == 0  && iy == 0) {
                    vector[ u ] = ( ix * width - width_half - dWidth ) * udir;
                } else if (udir == 1 && ix == 1  && iy == 0) {
                    vector[ u ] = ( ix * width - width_half + dWidth ) * udir;
                } else {
                    vector[ u ] = ( ix * width - width_half) * udir;
                }
                vector[ v ] = ( iy * height - height_half ) * vdir;
                vector[ w ] = depth;

                scope.vertices.push(vector);
            }
        }

        createFaces(offset, material);
    }

    function buildSkewPlane(udir, vdir, width, height, depth, dWidth, material) {

        var u = 'z', v = 'y', w = 'x';
        var w, ix, iy,
        width_half = width / 2,
        height_half = height / 2,
        offset = scope.vertices.length;

        for( iy = 0; iy < 2; iy++ ) {
            for( ix = 0; ix < 2; ix++ ) {
                var vector = new THREE.Vector3();
                vector[ u ] = ( ix * width - width_half ) * udir;
                vector[ v ] = ( iy * height - height_half ) * vdir;
                if (iy === 0) {
                    vector[ w ] = depth + dWidth;
                } else {
                    vector[ w ] = depth;
                }
                scope.vertices.push(vector);
            }
        }

        createFaces(offset, material);
        
    }
    
    function buildPlane( u, v, udir, vdir, width, height, depth, dWidth, material ) {

        var w, ix, iy,
        width_half = width / 2,
        height_half = height / 2,
        offset = scope.vertices.length;

        if ( ( u == 'x' && v == 'y' ) || ( u == 'y' && v == 'x' ) ) {
            w = 'z';
        } else if ( ( u == 'x' && v == 'z' ) || ( u == 'z' && v == 'x' ) ) {
            w = 'y';
        } else if ( ( u == 'z' && v == 'y' ) || ( u == 'y' && v == 'z' ) ) {
            w = 'x';
        }

        for( iy = 0; iy < 2; iy++ ) {

            for( ix = 0; ix < 2; ix++ ) {

                var vector = new THREE.Vector3();
                if (ix == 1 && u === 'x' && v === 'z' && vdir === 1) {
                    vector[ u ] = ( ix * width - width_half + dWidth) * udir;
                } else {
                    vector[ u ] = ( ix * width - width_half ) * udir;
                }
                vector[ v ] = ( iy * height - height_half ) * vdir;
                vector[ w ] = depth;

                scope.vertices.push(vector);
            }
        }
        createFaces(offset, material);

    }

    function createFaces(offset, material) {
        
        var a = 0;
        var b = 2;
        var c = 3;
        var d = 1;
        
        scope.faces.push( new THREE.Face4( a + offset, b + offset, c + offset, d + offset, null, null, material ) );
        scope.faceVertexUvs[ 0 ].push( [
            new THREE.UV( 0, 0 ),
            new THREE.UV( 0, 1 ),
            new THREE.UV( 1, 1 ),
            new THREE.UV( 1, 0 )
            ] );

    }


    function mergeVertices() {

        var unique = [], changes = [];

        for ( var i = 0, il = scope.vertices.length; i < il; i ++ ) {
            var v = scope.vertices[ i ],
            duplicate = false;
            for ( var j = 0, jl = unique.length; j < jl; j ++ ) {
                var vu = unique[ j ];
                if( v.x == vu.x && v.y == vu.y && v.z == vu.z ) {
                    changes[ i ] = j;
                    duplicate = true;
                    break;
                }
            }

            if ( ! duplicate ) {
                changes[ i ] = unique.length;
                unique.push(v.clone());
            }
        }

        for ( i = 0, il = scope.faces.length; i < il; i ++ ) {
            var face = scope.faces[ i ];

            face.a = changes[ face.a ];
            face.b = changes[ face.b ];
            face.c = changes[ face.c ];
            face.d = changes[ face.d ];
        }

        scope.vertices = unique;

    }

    this.computeCentroids();
    this.computeFaceNormals();

};

THREE.WedgeGeometry.prototype = new THREE.Geometry();
THREE.WedgeGeometry.prototype.constructor = THREE.WedgeGeometry;
