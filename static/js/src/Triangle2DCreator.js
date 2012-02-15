var SS = SS || {};

SS.Triangle2DCreator = SS.NodeModel.extend({

    initialize: function(attributes) {
        this.node = attributes.editingNode;
        this.editingNode = attributes.editingNode;

        var that = this;
        that.node.parameters.vertices.map(function(vertex) {
            vertex.u = 0;
            vertex.v = 0;
            vertex.w = 0;
        });
        that.node.parameters.vertices[1].u = 10;
        that.node.parameters.vertices[2].v = 10;

        this.views = [
            new SS.NodeDOMView({model: this}),
            new SS.Triangle2DPreview({model: this}),
            new SS.DraggableTriangleCorner({model: this, vertexIndex: 0}),
            new SS.DraggableTriangleCorner({model: this, vertexIndex: 1}),
            new SS.DraggableTriangleCorner({model: this, vertexIndex: 2}),
        ];

        geom_doc.on('replace', this.geomDocReplace, this);
        geom_doc.on('remove', this.geomDocRemove, this);
    },

    destroy: function() {
        if (this.activeCorner) {
            this.activeCornerView.remove();
        }
        this.views.map(function(view) {
            view.remove();
        });
        geom_doc.off('replace', this.geomDocReplace);
        geom_doc.off('remove', this.geomDocRemove);
    },

    updateFromDOMView: function() {
        SS.NodeModel.prototype.updateFromDOMView.call(this);
        this.trigger('change');
    },

    mouseDownOnCorner: function(corner) {
        if (this.activeCorner === corner) {
            return;
        }
        this.activeCorner && this.activeCornerView.remove();
        this.activeCorner = corner;
        this.activeCornerView = new SS.HeightCursoid({model: this,
                                                          vertexIndex: corner.vertexIndex});
    },
     
    tryCommit: function() {
        var cmd =  update_geom_command(this.originalNode, 
                                       this.editingNode, 
                                       this.editingNode); 
        command_stack.execute(cmd);
    },

    cancel: function() {
        this.destroy();
        geom_doc.remove(this.editingNode); 
    },

    geomDocReplace: function(original, replacement) {
        if (original === this.editingNode) {
            this.destroy();
        } 
    },

    geomDocRemove: function(node) {
        if (node === this.editingNode) {
            this.destroy();
        } 
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
        triangleGeometry.vertices = this.model.editingNode.parameters.vertices.map(function(vertex) {
            return new THREE.Vertex(new THREE.Vector3(vertex.u, vertex.v, vertex.w));
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

        this.postRender();
    },

});

SS.DraggableTriangleCorner = SS.ActiveTransformerView.extend({

     initialize: function(options) {
	 SS.ActiveTransformerView.prototype.initialize.call(this);
         this.vertexIndex = options.vertexIndex;
         this.on('mouseDown', this.mouseDown, this);
         this.on('mouseDrag', this.drag);
         this.render();
    },

    mouseDown: function() {
        this.model.mouseDownOnCorner && this.model.mouseDownOnCorner(this);
    },

    remove: function() {
        SS.ActiveTransformerView.prototype.remove.call(this);
        this.model.off('mouseDown', this.mouseDown);
        this.off('mouseDrag', this.drag);
    },

    render: function() {
        this.clear();

        var geometry = new THREE.CubeGeometry(0.5, 0.5, 0.5);
        var materials = [
            new THREE.MeshBasicMaterial({color: SS.constructors.faceColor, opacity: 0.5, wireframe: false } ),
            new THREE.MeshBasicMaterial({color: SS.constructors.lineColor, wireframe: true})
        ];
        var cube = THREE.SceneUtils.createMultiMaterialObject(geometry, materials);
        
        var vertex = this.model.node.parameters.vertices[this.vertexIndex];
	cube.position.x = vertex.u;
	cube.position.y = vertex.v;
	cube.position.z = vertex.w;
	this.sceneObject.add(cube);
        this.postRender();
        return this;
    },

    drag: function(event) {
        var workplanePosition = SS.sceneView.determinePositionOnWorkplane(event);
        var u = workplanePosition.x;
        var v = workplanePosition.y;

        var vertex = this.model.node.parameters.vertices[this.vertexIndex];

        vertex.u = parseFloat(u.toFixed(3));
        vertex.v = parseFloat(v.toFixed(3));

        if (!event.ctrlKey) {
            vertex.u = Math.round(vertex.u*10)/10;    
            vertex.v = Math.round(vertex.v*10)/10;
        }

        this.model.setParameters({});
    },

});

SS.HeightCursoid = SS.ActiveTransformerView.extend({

     initialize: function(options) {
	 SS.ActiveTransformerView.prototype.initialize.call(this);
         this.vertexIndex = options.vertexIndex;
         this.on('mouseDrag', this.drag);
         this.render();
    },

    remove: function() {
        SS.ActiveTransformerView.prototype.remove.call(this);
        this.off('mouseDrag', this.drag);
    },

    render: function() {
        this.clear();

        var axis = new THREE.Geometry();
        axis.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,-1000)));
        axis.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,1000)));
        var line = new THREE.Line(axis, new THREE.LineBasicMaterial({ color: 0xcc6666, opacity: 0.5 }));  

        var geometry = new THREE.CylinderGeometry(0, 0.5, 1, 3);
        var materials = [
            new THREE.MeshBasicMaterial({color: 0x993333, opacity: 0.5, wireframe: false } ),
            new THREE.MeshBasicMaterial({color: 0xcc6666, wireframe: true})
        ];
        var cursoids = [
            THREE.SceneUtils.createMultiMaterialObject(geometry, materials),
            THREE.SceneUtils.createMultiMaterialObject(geometry, materials)
        ];
        
        var vertex = this.model.node.parameters.vertices[this.vertexIndex];

        line.position.x = vertex.u;
        line.position.y = vertex.v;

	cursoids[0].position.x = vertex.u;
	cursoids[0].position.y = vertex.v;
	cursoids[0].position.z = vertex.w + 2;
        cursoids[0].rotation.x = Math.PI/2;

	cursoids[1].position.x = vertex.u;
	cursoids[1].position.y = vertex.v;
	cursoids[1].position.z = vertex.w - 2;
        cursoids[1].rotation.x = 3*Math.PI/2;
        
        this.sceneObject.add(line);
	this.sceneObject.add(cursoids[0]);
	this.sceneObject.add(cursoids[1]);
        this.postRender();
        return this;
    },

    drag: function(event) {

        var vertex = this.model.node.parameters.vertices[this.vertexIndex];

        var origin = new THREE.Vector3(vertex.u, vertex.v, 0);
	var direction = new THREE.Vector3(0, 0, 1);
	var ray = new THREE.Ray(origin, direction);
	var positionOnVertical = SS.sceneView.determinePositionOnRay(event, ray);

        vertex.w = parseFloat(positionOnVertical.z.toFixed(1));
        if (!event.ctrlKey) {
            vertex.w = Math.round(vertex.w);
        }

        this.model.setParameters({});
        
    },

});

