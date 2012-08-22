var SS = SS || {};

SS.FilletCreator = SS.ParentCreator.extend({
    
    initialize: function(attributes) { 
        attributes.noOriginCorner = true;
        SS.ParentCreator.prototype.initialize.call(this, attributes);
        
        if (this.originalNode) {
            this.boundingBox = SS.boundingBoxForGeomNode(this.editingNode);
        } else {
            this.node.parameters.r = 1.0;

            this.views.push(new SS.FilletGeomNodeView({model: this}));
            this.trigger('change:model', this);
        }
        this.views = this.views.concat([
            new SS.FilletRadiusDOMView({model: this}),
        ]);
        this.trigger('change', this);
    },
    
    mouseDownOnUV: function(corner) {
    },

    getBoundingBox: function() {
        if (this.boundingBox) {
            return {
                'min': this.boundingBox.min.clone(),
                'max': this.boundingBox.max.clone()
            }
        } else {
            return undefined;
        }
    },
    
});

SS.FilletGeomNodeView = SS.SceneObjectView.extend({

    initialize: function() {
        SS.SceneObjectView.prototype.initialize.call(this);
        this.model.on('change:model', this.render, this);
        this.meshes = SS.createGeometry(this.model.childNode);
        this.changeToPreviewColor(this.meshes);
        this.dontApplyWorkplane = true;
        this.render();
    },

    remove: function() {
        SS.SceneObjectView.prototype.remove.call(this);
        this.model.off('change:model', this.render);
    },

    render: function() {
        this.clear();
        this.sceneObject.add(this.meshes['faces']);
        this.sceneObject.add(this.meshes['edges']);

        this.model.boundingBox = SS.boundingBoxForSceneObject(this.sceneObject);
        this.model.center = SS.centerOfGeom(this.model.boundingBox);

        this.postRender();
    },
    
    changeToPreviewColor: function(meshes) {
        meshes.faces.children.map(function(child) {
            child.material = SS.materials.faceMaterial;
        });
        meshes.edges.children.map(function(child) {
            child.material = SS.materials.lineMaterial;

        });
    },

});

SS.FilletRadiusDOMView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change', this.update, this);
    },

    remove: function() {
        Backbone.View.prototype.remove.call(this);
        this.model.off('change', this.update);
    },

    render: function() {
        var text = this.model.node.parameters.text;
        var template = '<input type="number" class="field" id="floating-r" value="{{value}}"/>';
        this.$el.html($.mustache(template, {value: text}));
        $('#floating-dom-view .params').append(this.$el);
    },

    events: {
        'change .field': 'fieldChanged',
        'keyup .field': 'fieldChanged',
        'click .field': 'fieldChanged',
    },

    update: function() {
        $('#floating-r').val(this.model.node.parameters.r);
    },

    fieldChanged: function() {
        this.preventUpdate = true;
        this.model.setParameters({r: parseFloat($('#floating-r').val())});
        this.preventUpdate = false;
    },

});