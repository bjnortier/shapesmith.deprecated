var SS = SS || {};

SS.BezierCreator = SS.PrimitiveCreator.extend({

    initialize: function(attributes) {
        SS.PrimitiveCreator.prototype.initialize.call(this, attributes);

        this.views = this.views.concat([
            new SS.BezierPreview({model: this}),
            new SS.DraggableVertexCorner({model: this, vertexIndex: 0}),
            new SS.DraggableVertexCorner({model: this, vertexIndex: 1}),
            new SS.DraggableVertexCorner({model: this, vertexIndex: 2}), 
            new SS.DraggableVertexCorner({model: this, vertexIndex: 3}), 
            new SS.VertexDimensionText({model: this, vertexIndex: 0}),
            new SS.VertexDimensionText({model: this, vertexIndex: 1}),
            new SS.VertexDimensionText({model: this, vertexIndex: 2}), 
            new SS.VertexDimensionText({model: this, vertexIndex: 3}), 

        ]);
    },

    setDefaultParamters: function() {
        this.node.parameters.vertices.map(function(vertex) {
            vertex.u = 10;
            vertex.v = 0;
            vertex.w = 0;
        });
        this.node.parameters.vertices[1].u = 20;
        this.node.parameters.vertices[1].v = 10;
        this.node.parameters.vertices[2].u = 0;
        this.node.parameters.vertices[2].v = 10;
        this.node.parameters.vertices[3].u = 10;
        this.node.parameters.vertices[3].v = 20;

    },

    mouseDownOnCorner: function(corner) {
        this.activateCorner(corner, SS.VertexHeightCursoid, {
            model: this,
            vertexIndex: corner.vertexIndex
        });
    },
    
    getBoundingBox: function() {
        var origin = this.node.origin;
        var min = {}, max = {};
        this.node.parameters.vertices.map(function(vertex) {
            var map = {x: 'u', y: 'v', z: 'w'};
            for (key in map) {
                var distance = origin[key] + vertex[map[key]];
                min[key] = (min[key] && Math.min(distance, min[key])) || distance;
                max[key] = (max[key] && Math.max(distance, max[key])) || distance;
            }
        });
        return {min: new THREE.Vector3(min.x, min.y, min.z),
                max: new THREE.Vector3(max.x, max.y, max.z)};
    },

});



THREE.CubicBezierCurve3 = THREE.Curve.create(

    function ( v0, v1, v2, v3 ) {
        this.v0 = v0;
        this.v1 = v1;
        this.v2 = v2;
        this.v3 = v3;
    },

    function ( t ) {
        var tx, ty, tz;
        tx = THREE.Shape.Utils.b3( t, this.v0.x, this.v1.x, this.v2.x, this.v3.x );
        ty = THREE.Shape.Utils.b3( t, this.v0.y, this.v1.y, this.v2.y, this.v3.y );
        tz = THREE.Shape.Utils.b3( t, this.v0.z, this.v1.z, this.v2.z, this.v3.z );
        return new THREE.Vector3( tx, ty, tz );

    }

);

SS.BezierPreview = SS.PreviewWithOrigin.extend({

    initialize: function() {
        SS.SceneObjectView.prototype.initialize.call(this);
        this.render();
    },
    
    render: function() {
        this.clear();
        SS.PreviewWithOrigin.prototype.render.call(this);

        var vertices = this.model.node.parameters.vertices;
        var positions = vertices.map(function(vertex) {
            return new THREE.Vector3(vertex.u, vertex.v, vertex.w);
        });

        var constructor1 = new THREE.Geometry();
        constructor1.vertices.push(positions[0]);
        constructor1.vertices.push(positions[1]);
        this.sceneObject.add(new THREE.Line(constructor1, SS.materials.constructionLineMaterial));
        
        var constructor2 = new THREE.Geometry();
        constructor2.vertices.push(positions[2]);
        constructor2.vertices.push(positions[3]);
        this.sceneObject.add(new THREE.Line(constructor2, SS.materials.constructionLineMaterial));

        var curve = new THREE.CubicBezierCurve3(positions[0], positions[1], positions[2], positions[3]);
        var baseGeometry = new THREE.Geometry();
        for (var t = 0; t <= 100; ++t) {
            baseGeometry.vertices.push(curve.getPoint(t/100));
        }
        var bezierCurve = new THREE.Line(baseGeometry, SS.materials.lineMaterial);
        this.sceneObject.add(bezierCurve);

        this.postRender();
    },

    priority: -1,

});