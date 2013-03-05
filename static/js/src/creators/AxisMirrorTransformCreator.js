var SS = SS || {};

SS.AxisMirrorTransformCreator = SS.TransformCreator.extend({

    initialize: function(attributes) {
        SS.TransformCreator.prototype.initialize.call(this, attributes);
        this.node.parameters = {u: 0, v: 0, w: 1, n:0};

        this.views = this.views.concat([
            new SS.AxisMirrorTransformPreview({model: this}),
            new SS.XYZAxisChoice({model: this}),
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

SS.AxisMirrorTransformPreview = SS.PreviewWithOrigin.extend({

    initialize: function() {
        SS.PreviewWithOrigin.prototype.initialize.call(this);
        this.render();
    },

    render: function() {
        if (this.model.originalNode.originalSceneObjects) {
            var transform = this.model.node;
            var position = SS.objToVector(transform.parameters);
            SS.axisMirrorGeomNodeRendering(this.model.originalNode, 
                                           this.model.editingNode, 
                                           this.model.node);
        }

        this.clear();
        SS.PreviewWithOrigin.prototype.render.call(this);

        var axisVector = SS.objToVector(this.model.node.parameters).normalize();
        
        var axis = new THREE.Geometry();
        axis.vertices.push(axisVector.clone().multiplyScalar(1000));
        axis.vertices.push(axisVector.clone().multiplyScalar(-1000));
        var line = new THREE.Line(axis, SS.materials.lineMaterial);  
        this.sceneObject.add(line);

        this.postRender();
    },

});


SS.XYZAxisChoice = Backbone.View.extend({

    initialize: function() {
        this.render();
    },

    remove: function() {
        Backbone.View.prototype.remove.call(this);
    },

    render: function() {
        var table = '<table><tr><td><input class="X-axis" type="submit" value="X"/><input class="Y-axis" type="submit" value="Y"/><input class="Z-axis" type="submit" value="Z"/></td></tr></table>';
        this.$el.html(table);
        $('#floating-dom-view').prepend(this.$el);
    },

    events: {
        'click .X-axis' : 'xaxis',
        'click .Y-axis' : 'yaxis',
        'click .Z-axis' : 'zaxis',
    },

    xaxis: function() {
        this.model.setParameters({u: 1, v: 0, w: 0});
    },

    yaxis: function() {
        this.model.setParameters({u: 0, v: 1, w: 0});
    },

    zaxis: function() {
        this.model.setParameters({u: 0, v: 0, w: 1});
    },


});
