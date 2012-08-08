var SS = SS || {};

SS.PolylineCreator = SS.PrimitiveCreator.extend({

    initialize: function(attributes) {
        SS.PrimitiveCreator.prototype.initialize.call(this, attributes);

        this.views = this.views.concat([
            new SS.PolylinePreview({model: this}),
            new SS.AddRemovePointChoice({model: this}),
        ]);
        
        for (var i = 0; i < this.node.parameters.vertices.length; ++i) {
            this.views.push(new SS.DraggableVertexCorner({model: this, vertexIndex: i}));
            this.views.push(new SS.VertexDimensionText({model: this, vertexIndex: i}));
        }
    },

    setDefaultParamters: function() {
        this.node.parameters.vertices[0] = {u: 10, v: 0, w: 0};
        this.node.parameters.vertices[1] = {u: 0, v: 10, w: 0};

    },

    addPoint:function() {
        var vertices = this.node.parameters.vertices;
        var lengthBefore = vertices.length;
        var last = vertices[lengthBefore - 1];
        vertices.push({u: last.u + 10, v: last.v + 10, w: last.w});
        this.setParameters({});
        this.nodeDOMView.render();

        this.views.push(new SS.DraggableVertexCorner({model: this, vertexIndex: lengthBefore}));
        this.views.push(new SS.VertexDimensionText({model: this, vertexIndex: lengthBefore}));
    },

    removePoint:function() {
        this.activeCornerView && this.activeCornerView.remove();
        
        this.views.pop().remove();
        this.views.pop().remove();

        var vertices = this.node.parameters.vertices;
        vertices.pop();
        this.setParameters({});
        this.nodeDOMView.render();
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

SS.PolylinePreview = SS.PreviewWithOrigin.extend({

    initialize: function() {
        SS.SceneObjectView.prototype.initialize.call(this);
        this.render();
    },
    
    render: function() {
        this.clear();
        SS.PreviewWithOrigin.prototype.render.call(this);

        var polylineGeometry = new THREE.Geometry();
        var origin = this.model.node.origin;
        var vertices = this.model.node.parameters.vertices;

        polylineGeometry.vertices = vertices.map(function(vertex) {
            return new THREE.Vector3(vertex.u, vertex.v, vertex.w);
        });
        
        var polyline = new THREE.Line(polylineGeometry, SS.materials.lineMaterial);
        this.sceneObject.add(polyline);

        var that = this;
        vertices.map(function(vertex) {
            var vertexPointGeometry = new THREE.CubeGeometry(0.2, 0.2, 0.2);
            var vertexPointMaterials = [
                new THREE.MeshBasicMaterial({color: 0xcccccc, opacity: 0.5, wireframe: false } ),
                new THREE.MeshBasicMaterial({color: 0x999999, wireframe: true})
            ];
            var vertexPoint = THREE.SceneUtils.createMultiMaterialObject(vertexPointGeometry, vertexPointMaterials);
            vertexPoint.position = new THREE.Vector3(vertex.u, vertex.v, vertex.w);
            that.sceneObject.add(vertexPoint);
        });

        if (origin.z) {
            var baseGeometry = new THREE.Geometry();
            var verticesForBase = triangleVertices.concat([triangleVertices[0]])
            baseGeometry.vertices = verticesForBase.map(function(vertex) {
                return new THREE.Vector3(vertex.u, vertex.v, 0);
            });
            var base = new THREE.Line(baseGeometry, SS.materials.lineMaterial);
            base.position.z = -origin.z;
            this.sceneObject.add(base);
        }

        this.postRender();
    },

    priority: -1,

});

SS.AddRemovePointChoice = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change', this.render, this);
    },

    remove: function() {
        Backbone.View.prototype.remove.call(this);
        this.model.off('change', this.update);
    },

    render: function() {
        var minLength = SS.schemas.polyline.properties.parameters.properties.vertices.minItems;
        var length = this.model.node.parameters.vertices.length;
        var table = '<table><tr><td><input class="add-point" type="submit" value="+"/>';
        if (length > minLength) {
            table += '<input class="remove-point" type="submit" value="-"/>';
        }
        table += '</td></tr></table>';
        this.$el.html(table);
        $('#floating-dom-view').prepend(this.$el);
    },

    events: {
        'click .add-point' : 'addPoint',
        'click .remove-point' : 'removePoint'
    },

    addPoint: function() {
        this.model.addPoint();
    },

    removePoint: function() {
        this.model.removePoint();
    },

});
