var SS = SS || {};

SS.Ellipse1DCreator = SS.PrimitiveCreator.extend({

    initialize: function(attributes) {
        SS.PrimitiveCreator.prototype.initialize.call(this, attributes);

        this.views = this.views.concat([
            new SS.EllipsePreview({model: this}),
            new SS.DraggableUVCorner({model: this, uKey: 'r1', vKey: 'r2'}),
            new SS.Ellipse2DDimensionArrows({model: this}),
            new SS.Ellipse2DDimensionText({model: this}),
            new SS.DraggableAngleCorner({model: this, key: 'from_angle', priority: 2}),
            new SS.DraggableAngleCorner({model: this, key: 'to_angle', priority: 3}),
        ]);
        this.trigger('change', this);
    },

    setDefaultParamters: function() {
        this.node.parameters.r1 = 20;
        this.node.parameters.r2 = 10;
        this.node.parameters.from_angle = 0;
        this.node.parameters.to_angle = 360;
    },

    mouseDownOnUV: function(corner) {
        this.activateCorner(corner);
    },

    getBoundingBox: function() {
        var origin = this.node.origin;
        var r1 = this.node.parameters.r1;
        var r2 = this.node.parameters.r2;
        return {min: new THREE.Vector3(origin.x - r1, origin.y - r2, origin.z),
                max: new THREE.Vector3(origin.x + r1, origin.y + r2, origin.z)};
    },

});

SS.Ellipse1DPreview = SS.EllipsePreview.extend({

    initialize: function() {
        SS.EllipsePreview.prototype.initialize.call(this);
        this.render();
    },
    
    render: function() {
        this.clear();
        SS.EllipsePreview.prototype.render.call(this);

        this.postRender();
    },

});

SS.DraggableAngleCorner = SS.DraggableCorner.extend({

    initialize: function(options) {
        SS.DraggableCorner.prototype.initialize.call(this, options);
        this.render();
        this.priority = options.priority || 1;
    },

    mouseDown: function() {
    },

    cornerPositionFromModel: function() {
        var r1 = this.model.node.parameters.r1;
        var r2 = this.model.node.parameters.r2;
        var angle = this.model.node.parameters[this.options.key];
        var dx = Math.cos(angle/180*Math.PI)*r1;
        var dy = Math.sin(angle/180*Math.PI)*r2;
        return {x: this.model.node.origin.x + dx,
                y: this.model.node.origin.y + dy,
                z: this.model.node.origin.z};
    },

    updateModelFromCorner: function(position) {
        var r1 = this.model.node.parameters.r1;
        var r2 = this.model.node.parameters.r2;
        var dx = position.x - this.model.node.origin.x;
        var dy = position.y - this.model.node.origin.y;
        var angle = Math.atan2(dy, dx*r2/r1);

        this.model.node.parameters[this.options.key] = Math.round(angle/Math.PI*180);
    },

});

