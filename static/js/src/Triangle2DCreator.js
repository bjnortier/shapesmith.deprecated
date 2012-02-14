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
        ];
    },
    
    destroy: function() {
        this.views.map(function(view) {
            view.remove();
        });
        geom_doc.off('remove', this.geomDocRemove);
    },

    updateFromDOMView: function() {
        SS.NodeModel.prototype.updateFromDOMView.call(this);
        this.trigger('change');
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

