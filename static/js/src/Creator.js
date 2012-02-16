var SS = SS || {};

SS.Creator = SS.NodeModel.extend({

    initialize: function(attributes) {
        this.node = attributes.editingNode;
        this.editingNode = attributes.editingNode;

        this.views = [
            new SS.NodeDOMView({model: this}),
            new SS.OkCancelView({model: this}),
            new SS.DraggableOriginCorner({model: this}),
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

    activateCorner: function(corner, constructor, args) {
        if (this.activeCorner === corner) {
            return;
        }
        this.activeCornerView && this.activeCornerView.remove();
        this.activeCorner = corner;
        if (constructor) {
            this.activeCornerView = new constructor(args || {model: this});
        }
    },

    mouseDownOnOrigin: function(corner) {
        this.activateCorner(corner, SS.OriginHeightCursoid);
    },
     
    tryCommit: function() {
        var cmd = create_geom_command(this.node, {type: this.node.type,
						 origin: this.node.origin,
                                                 parameters: this.node.parameters});
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

SS.PreviewWithOrigin = SS.SceneObjectView.extend({

    initialize: function() {
        SS.SceneObjectView.prototype.initialize.call(this);
    },
    
    render: function() {
        var origin = this.model.node.origin;
        
        var vertexGeometry = new THREE.CubeGeometry(0.1, 0.1, 0.1);
        var vertexMaterials = [
            new THREE.MeshBasicMaterial({color: 0xcccccc, opacity: 0.5, wireframe: false } ),
            new THREE.MeshBasicMaterial({color: 0x999999, wireframe: true})
        ];
        var originCorner = THREE.SceneUtils.createMultiMaterialObject(vertexGeometry, vertexMaterials);
        originCorner.position = new THREE.Vector3(origin.x, origin.y, origin.z);
        this.sceneObject.add(originCorner);
        
    },

    priority: -1,

});

SS.DraggableCorner = SS.ActiveTransformerView.extend({

     initialize: function(options) {
	 SS.ActiveTransformerView.prototype.initialize.call(this);
         this.on('mouseDown', this.mouseDown, this);
         this.on('mouseDrag', this.drag);
    },

    remove: function() {
        SS.ActiveTransformerView.prototype.remove.call(this);
        this.model.off('mouseDown', this.mouseDown);
        this.off('mouseDrag', this.drag);
    },

    render: function() {
        this.clear();

        var geometry = new THREE.CubeGeometry(0.75, 0.75, 0.75);
        var materials = [
            new THREE.MeshBasicMaterial({color: SS.constructors.faceColor, opacity: 0.5, wireframe: false } ),
            new THREE.MeshBasicMaterial({color: SS.constructors.lineColor, wireframe: true})
        ];
        var cube = THREE.SceneUtils.createMultiMaterialObject(geometry, materials);
        
        var position = this.cornerPositionFromModel();

	cube.position.x = position.x
	cube.position.y = position.y;
	cube.position.z = 0;
	this.sceneObject.add(cube);
        this.postRender();
        return this;
    },

    drag: function(event) {
        var workplanePosition = SS.sceneView.determinePositionOnWorkplane(event);
        var u = workplanePosition.x;
        var v = workplanePosition.y;

        this.updateModelFromCorner(workplanePosition);
        this.model.setParameters({});
   
    },

});

SS.DraggableOriginCorner = SS.DraggableCorner.extend({

    initialize: function(options) {
        SS.DraggableCorner.prototype.initialize.call(this, options);
        this.render();
    },

    mouseDown: function() {
        this.model.mouseDownOnOrigin(this);
    },

    cornerPositionFromModel: function() {
        var origin = this.model.node.origin;
        return {x: origin.x,
                y: origin.y,
                z: origin.z};
    },
    
    updateModelFromCorner: function(position) {
        var origin = this.model.node.origin;
        origin.x = position.x;
        origin.y = position.y;
    },

});

SS.HeightCursoid = SS.ActiveTransformerView.extend({

     initialize: function(options) {
	 SS.ActiveTransformerView.prototype.initialize.call(this);
         this.on('mouseDrag', this.drag);
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
        var cursoid = THREE.SceneUtils.createMultiMaterialObject(geometry, materials);

        var position = this.cornerPositionFromModel();

        line.position.x = position.x;
        line.position.y = position.y;

	cursoid.position.x = position.x;
	cursoid.position.y = position.y;
	cursoid.position.z = position.z + 1.5;
        cursoid.rotation.x = Math.PI/2;
        
        this.sceneObject.add(line);
	this.sceneObject.add(cursoid);
        this.postRender();
        return this;
    },

    drag: function(event) {

        var position = this.cornerPositionFromModel();

        var origin = new THREE.Vector3(position.x, position.y, 0);
	var direction = new THREE.Vector3(0, 0, 1);
	var ray = new THREE.Ray(origin, direction);
	var positionOnVertical = SS.sceneView.determinePositionOnRay(event, ray);
        if (positionOnVertical) {
            
            if (!event.ctrlKey) {
                positionOnVertical.x = Math.round(positionOnVertical.x);
                positionOnVertical.y = Math.round(positionOnVertical.y);
                positionOnVertical.z = Math.round(positionOnVertical.z);
            }
            this.updateModelFromCorner(positionOnVertical);
            this.model.setParameters({});
        }
    },

});

SS.OriginHeightCursoid = SS.HeightCursoid.extend({

     initialize: function(options) {
	 SS.HeightCursoid.prototype.initialize.call(this);
         this.render();
    },

    cornerPositionFromModel: function() {
        return {x: this.model.node.origin.x,
                y: this.model.node.origin.y,
                z: this.model.node.origin.z};
    },    

    updateModelFromCorner: function(position) {
        this.model.node.origin.x = position.x;
        this.model.node.origin.y = position.y;
        this.model.node.origin.z = position.z;
    },


});