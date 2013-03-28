define([
        'jquery',
        'lib/jquery.mustache',
        'calculations',
        'worldcursor',
        'scene',
        'geometrygraphsingleton',
        'modelviews/geomvertexMV', 
        'pointMV', 
        'asyncAPI',
    ], 
    function(
        $, __$,
        calc,
        worldCursor,
        sceneModel,
        geometryGraph,
        GeomVertexMV,
        PointMV,
        AsyncAPI) {

    // ---------- Common ----------

    var SceneViewMixin = {

        render: function() {
            GeomVertexMV.SceneView.prototype.render.call(this);

            var points = geometryGraph.childrenOf(this.model.vertex).filter(function(v) {
                return v.type === 'point'
            });
            if (points.length !== 1) {
                return;
            }

            var faceMaterial = this.model.vertex.editing ? 
                this.materials.editing.face :
                this.materials.normal.face;

            var center = calc.objToVector(points[0].parameters.coordinate, geometryGraph, THREE.Vector3);
            var radius = geometryGraph.evaluate(this.model.vertex.parameters.radius);

            if (radius > 0) {
                var sphere = THREE.SceneUtils.createMultiMaterialObject(
                    new THREE.SphereGeometry(radius, 20, 20),
                    [faceMaterial]);
                sphere.position = center;
                sphere.rotation.x = Math.PI/2;
                this.sceneObject.add(sphere);
            }

            var circleGeom = new THREE.Geometry();
            for(var i = 0; i <= 50; ++i) {
                var theta = Math.PI*2*i/50;
                var dx = radius*Math.cos(theta);
                var dy = radius*Math.sin(theta);
                circleGeom.vertices.push(new THREE.Vector3(dx, dy, 0));
            }
            var circle;
            if (this.model.vertex.editing) {
                circle = new THREE.Line(circleGeom, this.materials.editing.edge);
            } else {
                circle = new THREE.Line(circleGeom, this.materials.normal.edge);
            }
            circle.position = center;
            this.sceneObject.add(circle);

        },
    }

    // ---------- Editing ----------

    var EditingModel = GeomVertexMV.EditingModel.extend({

        initialize: function(options) {
            this.displayModelConstructor = DisplayModel;
            this.DOMView = EditingDOMView;
            this.SceneView = EditingSceneView;
            GeomVertexMV.EditingModel.prototype.initialize.call(this, options);

            var points = geometryGraph.childrenOf(this.vertex).filter(function(v) {
                return v.type === 'point'
            });

            // Create the child models
            var that = this;
            if (this.vertex.proto) {
                this.stage = 'center';
                this.updateHint();
                this.activePoint = points[0];
            } else {
                this.originalImplicitChildren = geometryGraph.childrenOf(this.vertex);
                this.editingImplicitChildren = [];
                this.editingImplicitChildren = this.originalImplicitChildren.map(function(child, i) {
                    var editing = AsyncAPI.edit(child);
                    return editing;
                })
            }

        },

        workplanePositionChanged: function(position, event) {
            if (this.vertex.proto) {
                // Center
                if (this.stage === 'center') {
                    this.activePoint.parameters.coordinate.x = position.x;
                    this.activePoint.parameters.coordinate.y = position.y;
                    this.activePoint.parameters.coordinate.z = position.z;
                    this.activePoint.trigger('change', this.activePoint);  
                // Radius          
                } else if (this.stage === 'radius') {
                    var points = geometryGraph.childrenOf(this.vertex).filter(function(v) {
                        return v.type === 'point'
                    });
                    var center = 
                        calc.objToVector(points[0].parameters.coordinate, geometryGraph, THREE.Vector3);
                    var radius = Math.sqrt(
                        Math.pow(center.x-position.x, 2) +
                        Math.pow(center.y-position.y, 2) +
                        Math.pow(center.z-position.z, 2));
                    this.vertex.parameters.radius = radius;
                    this.vertex.trigger('change', this.vertex);
                }
            }
        },

        sceneViewClick: function(viewAndEvent) {
            if (this.vertex.proto) {
                this.workplaneClick(worldCursor.lastPosition);
            }
        },

        workplaneClick: function(position) {
            if (this.vertex.proto) {
                if (this.stage === 'center') {
                    this.stage ='radius';
                    this.updateHint();
                } else if (this.stage === 'radius') {
                    this.tryCommit();
                }
            } else {
                this.tryCommit();
            }
        },

        updateHint: function() {
            if (this.vertex.proto) {
                switch(this.stage) {
                    case 0: 
                        this.hintView.set('Click to add a corner.');
                        break;
                    case 1:
                        this.hintView.set('Click to set the radius.');
                        break;
                }
            }
        },

    });

    var EditingDOMView = GeomVertexMV.EditingDOMView.extend({

        render: function() {
            GeomVertexMV.EditingDOMView.prototype.render.call(this);
            var template = 
                this.beforeTemplate +
                '<div>radius <input class="field radius" type="text" value="{{radius}}"></input></div>' +
                this.afterTemplate;
            var view = _.extend(this.baseView, {
                radius : this.model.vertex.parameters.radius,
            });
            this.$el.html($.mustache(template, view));
            return this;
        },

        update: function() {
            var that = this;
            ['radius'].forEach(function(key) {
                that.$el.find('.field.' + key).val(
                    that.model.vertex.parameters[key]);
            });
        },

        updateFromDOM: function() {
            var that = this;
            ['radius'].forEach(function(key) {
                try {
                    var expression = that.$el.find('.field.' + key).val();
                    that.model.vertex.parameters[key] = expression;
                } catch(e) {
                    console.error(e);
                }
            });
            this.model.vertex.trigger('change', this.model.vertex);
        }

    }); 


    var EditingSceneView = GeomVertexMV.EditingSceneView.extend(SceneViewMixin).extend({

        initialize: function(options) {
            GeomVertexMV.EditingSceneView.prototype.initialize.call(this);
            this.on('dragEnded', this.dragEnded, this);
            this.on('drag', this.drag, this);
        },

        remove: function() {
            GeomVertexMV.EditingSceneView.prototype.remove.call(this);
            this.off('dragEnded', this.dragEnded, this);
            this.off('drag', this.drag, this);
        },


    });

    // ---------- Display ----------

    var DisplayModel = GeomVertexMV.DisplayModel.extend({

        initialize: function(options) {
            this.editingModelConstructor = EditingModel;
            this.displayModelConstructor = DisplayModel;
            this.SceneView = DisplaySceneView;
            GeomVertexMV.DisplayModel.prototype.initialize.call(this, options);
        },

        destroy: function() {
            GeomVertexMV.DisplayModel.prototype.destroy.call(this);
        },


    }); 

    var DisplaySceneView = GeomVertexMV.DisplaySceneView.extend(SceneViewMixin).extend({

        render: function() {
            GeomVertexMV.DisplaySceneView.prototype.render.call(this);
            var that = this;
            this.createMesh(function(result) {
                that.renderMesh(result);
            });
        },

    })

    // ---------- Module ----------

    return {
        EditingModel: EditingModel,
        DisplayModel: DisplayModel,
    }

});