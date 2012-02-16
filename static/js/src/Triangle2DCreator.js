var SS = SS || {};

SS.Triangle2DCreator = SS.Creator.extend({

    initialize: function(attributes) {
        SS.Creator.prototype.initialize.call(this, attributes);

        this.node.parameters.vertices.map(function(vertex) {
            vertex.u = 0;
            vertex.v = 0;
            vertex.w = 0;
        });
        this.node.parameters.vertices[1].u = 10;
        this.node.parameters.vertices[2].v = 10;

        this.views = this.views.concat([
            new SS.Triangle2DPreview({model: this}),
            new SS.DraggableVertexCorner({model: this, vertexIndex: 0}),
            new SS.DraggableVertexCorner({model: this, vertexIndex: 1}),
            new SS.DraggableVertexCorner({model: this, vertexIndex: 2}), 
        ]);
    },

    mouseDownOnCorner: function(corner) {
        activateCorner(corner, SS.VertexHeightCursoid, {
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

SS.Triangle2DPreview = SS.SceneObjectView.extend({

    initialize: function() {
        SS.SceneObjectView.prototype.initialize.call(this);
        this.render();
    },
    
    render: function() {
        this.clear();

        var triangleGeometry = new THREE.Geometry();
        var origin = this.model.node.origin;
        triangleGeometry.vertices = this.model.node.parameters.vertices.map(function(vertex) {
            return new THREE.Vertex(new THREE.Vector3(origin.x + vertex.u, 
                                                      origin.y + vertex.v, 
                                                      origin.z + vertex.w));
        });
        triangleGeometry.faces.push(new THREE.Face3(0,1,2));
        
        var triangleFace = new THREE.Mesh(triangleGeometry, 
                                          new THREE.MeshBasicMaterial({color: SS.constructors.faceColor, 
                                                                       transparent: true, 
                                                                       opacity: 0.5}));
        triangleFace.doubleSided = true;
        var triangleWireframe = new THREE.Mesh(triangleGeometry, 
                                               new THREE.MeshBasicMaterial({color: SS.constructors.lineColor, wireframe: true}));

	this.sceneObject.add(triangleFace);
	this.sceneObject.add(triangleWireframe);

        var vertexGeometry = new THREE.CubeGeometry(0.1, 0.1, 0.1);
        var vertexMaterials = [
            new THREE.MeshBasicMaterial({color: 0xcccccc, opacity: 0.5, wireframe: false } ),
            new THREE.MeshBasicMaterial({color: 0x999999, wireframe: true})
        ];

        var originCorner = THREE.SceneUtils.createMultiMaterialObject(vertexGeometry, vertexMaterials);
        originCorner.position = new THREE.Vector3(origin.x, origin.y, origin.z);
        this.sceneObject.add(originCorner);

        var that = this;
        triangleGeometry.vertices.map(function(vertex) {
            var corner = THREE.SceneUtils.createMultiMaterialObject(vertexGeometry, vertexMaterials);
            corner.position = vertex.position;
            that.sceneObject.add(corner);
        });
        

        this.postRender();
    },

    priority: -1,

});


SS.DraggableVertexCorner = SS.DraggableCorner.extend({

    initialize: function(options) {
        SS.DraggableCorner.prototype.initialize.call(this, options);
        this.vertexIndex = options.vertexIndex;
        this.render();
    },

    priority: 1,

    mouseDown: function() {
        this.model.mouseDownOnCorner(this);
    },

    cornerPositionFromModel: function() {
        var vertex = this.model.node.parameters.vertices[this.vertexIndex];
        return {x: this.model.node.origin.x + vertex.u,
                y: this.model.node.origin.y + vertex.v,
                z: this.model.node.origin.z + vertex.w};
    },

    updateModelFromCorner: function(position) {
        var vertex = this.model.node.parameters.vertices[this.vertexIndex];
        var origin = this.model.node.origin;
        vertex.u = position.x - origin.x;
        vertex.v = position.y - origin.y;
    },

});

SS.VertexHeightCursoid = SS.HeightCursoid.extend({

     initialize: function(options) {
	 SS.HeightCursoid.prototype.initialize.call(this);
         this.vertexIndex = options.vertexIndex;
         this.render();
    },

    cornerPositionFromModel: function() {
        var vertex = this.model.node.parameters.vertices[this.vertexIndex];
        return {x: this.model.node.origin.x + vertex.u,
                y: this.model.node.origin.y + vertex.v,
                z: this.model.node.origin.z + vertex.w};
    },    

    updateModelFromCorner: function(position) {
        var vertex = this.model.node.parameters.vertices[this.vertexIndex];
        var origin = this.model.node.origin;
        vertex.u = position.x - origin.x;
        vertex.v = position.y - origin.y;
        vertex.w = position.z - origin.z;
    },

});
