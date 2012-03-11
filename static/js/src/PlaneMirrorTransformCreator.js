var SS = SS || {};

SS.PlaneMirrorTransformCreator = SS.TransformCreator.extend({

    initialize: function(attributes) {
        SS.TransformCreator.prototype.initialize.call(this, attributes);
        this.node.parameters = {u: 0, v: 0, w: 1, n:0};

        this.views = this.views.concat([
            new SS.PlaneMirrorTransformPreview({model: this}),
            new SS.PlaneChoice({model: this}),
        ]);
        this.trigger('change', this);
    },

    getBoundingBox: function() {
        if (!this.attributes.editingExisting) {
            return SS.boundingBoxForGeomNode(this.editingNode);
        } else {
            return undefined;
        }
    },

});

SS.PlaneMirrorTransformPreview = SS.PreviewWithOrigin.extend({

    initialize: function() {
        SS.PreviewWithOrigin.prototype.initialize.call(this);
        this.render();
    },

    render: function() {
        if (this.model.originalNode.originalSceneObjects) {
            var transform = this.model.node;
            var position = new THREE.Vector3(transform.parameters.u,
                                             transform.parameters.v,
                                             transform.parameters.w);
            

            SS.planeMirrorGeomNodeRendering(this.model.originalNode, 
                                            this.model.editingNode, 
                                            this.model.node);
        }

        this.clear();
        SS.PreviewWithOrigin.prototype.render.call(this);

        var origin = this.model.node.origin;
        var u = this.model.node.parameters.u;
        var v = this.model.node.parameters.v;
        var w = this.model.node.parameters.w;

        var axisVector = new THREE.Vector3(u,v,w).normalize();
        
        var planeGeometry = new THREE.PlaneGeometry(120, 120);
        var plane = new THREE.Mesh(planeGeometry, SS.materials.faceMaterial);  
        plane.lookAt(new THREE.Vector3(u,v,w));
        plane.doubleSided = true;
        this.sceneObject.add(plane);

        this.postRender();
    },

});


SS.PlaneChoice = Backbone.View.extend({

    initialize: function() {
        this.render();
    },

    remove: function() {
        Backbone.View.prototype.remove.call(this);
    },

    render: function() {
        var table = '<table><tr><td><input class="XY" type="submit" value="XY"/><input class="YZ" type="submit" value="YZ"/><input class="ZX" type="submit" value="ZX"/></td></tr></table>';
        this.$el.html(table);
        $('#floating-dom-view').prepend(this.$el);
    },

    events: {
        'click .XY' : 'xy',
        'click .YZ' : 'yz',
        'click .ZX' : 'zx',
    },

    xy: function() {
        this.model.node.parameters.u = 0;
        this.model.node.parameters.v = 0;
        this.model.node.parameters.w = 1;
        this.model.setParameters({});
    },

    yz: function() {
        this.model.node.parameters.u = 1;
        this.model.node.parameters.v = 0;
        this.model.node.parameters.w = 0;
        this.model.setParameters({});
    },

    zx: function() {
        this.model.node.parameters.u = 0;
        this.model.node.parameters.v = 1;
        this.model.node.parameters.w = 0;
        this.model.setParameters({});
    },


});
