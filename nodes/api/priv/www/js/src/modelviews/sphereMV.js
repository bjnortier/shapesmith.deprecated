define([
        'underscore',
        'jquery',
        'lib/jquery.mustache',
        'src/calculations',
        'src/settings',
        'src/scene',
        'src/scenevieweventgenerator',
        'src/worldcursor',
        'src/geometrygraphsingleton',
        'src/vertexMV',
        'src/geomvertexMV', 
        'src/pointMV', 
        'src/asyncAPI',
    ], 
    function(
        _,
        $, __$,
        calc,
        settings,
        sceneModel,
        sceneViewEventGenerator,
        worldCursor,
        geometryGraph,
        VertexMV,
        GeomVertexMV,
        PointMV,
        AsyncAPI) {

    // ---------- Common ----------

    var SceneViewMixin = {

        render: function() {
            GeomVertexMV.SceneView.prototype.render.call(this);

            var points = geometryGraph.childrenOf(this.model.vertex);
            if (points.length !== 1) {
                return;
            }

            var materials;
            if (this.model.vertex.editing) {
                materials = [
                    this.materials.editing.face, 
                    // this.materials.editing.wire
                ]
            } else {
                materials = [
                    this.materials.normal.face, 
                    // this.materials.normal.wire
                ]
            }

            var center = calc.objToVector(points[0].parameters.coordinate, geometryGraph, THREE.Vector3);
            var radius = geometryGraph.evaluate(this.model.vertex.parameters.radius);

            if (radius > 0) {
                var sphere = THREE.SceneUtils.createMultiMaterialObject(
                    new THREE.SphereGeometry(radius, 20, 20),
                    materials);
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
            GeomVertexMV.EditingModel.prototype.initialize.call(this, options);

            var points = geometryGraph.childrenOf(this.vertex);

            this.faceGroup = options.faceGroup;
            this.domView = new EditingDOMView({model: this});
            this.views.push(this.domView);

            // Create the child models
            var that = this;
            if (this.vertex.proto) {
                this.stage = 0;
                this.updateHint();
                this.subModels = [
                    new PointMV.EditingModel({
                        vertex: points[0],
                        parentModel: this,
                    }),
                ];
                this.activePoint = this.subModels[0];
            } else {
                this.originalImplicitChildren = geometryGraph.childrenOf(this.vertex);
                this.editingImplicitChildren = [];
                this.editingImplicitChildren = this.originalImplicitChildren.map(function(child, i) {
                    var editing = AsyncAPI.edit(child);
                    return editing;
                })
                this.subModels = this.originalImplicitChildren.map(function(child, i) {
                    // Replace the original model with an editing model
                    var modelToDestroy = VertexMV.getModelForVertex(child)
                    modelToDestroy.destroy();
                    return new modelToDestroy.editingModelConstructor({
                        original: child,
                        vertex: that.editingImplicitChildren[i],
                        parentModel: that
                    });
                });
            }

            this.setMainSceneView(new EditingSceneView({model: this}));
        },

        workplanePositionChanged: function(position, event) {
            if (this.vertex.proto) {
                if (this.stage === 0) {
                    this.activePoint.vertex.parameters.coordinate.x = position.x;
                    this.activePoint.vertex.parameters.coordinate.y = position.y;
                    this.activePoint.vertex.parameters.coordinate.z = position.z;
                    this.activePoint.vertex.trigger('change', this.activePoint.vertex);            
                } else if (this.stage === 1) {
                    var points = geometryGraph.childrenOf(this.vertex);
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
                if (this.stage === 0) {
                    ++this.stage;
                    this.updateHint();
                } else if (this.stage === 1) {
                    this.tryCommit();
                }
            } else {
                this.tryCommit();
            }
        },

        isChildClickable: function(childModel) {
            // Can't click the active point
            return childModel !== this.activePoint;
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
            var template = 
                '<table><tr>' +
                '<td class="title">' + 
                '<img src="/ui/images/icons/sphere24x24.png"/>' +
                '<div class="name">{{name}}</div>' + 
                '<div class="delete"></div>' + 
                '</td></tr><tr><td>' +
                '</div>' + 
                '<div>' + 
                'radius <input class="field radius" type="text" value="{{radius}}"></input>' +
                '</div>' +
                '<div class="points">' + 
                '</div>' + 
                '</td></tr></table>';
            var view = {
                name      : this.model.vertex.name,
                radius    : this.model.vertex.parameters.radius,
            };
            this.$el.html($.mustache(template, view));
            return this;
        },

        insertChild: function(childModel, childElement) {
            this.$el.find('.points').append(childElement);
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
            GeomVertexMV.DisplayModel.prototype.initialize.call(this, options);

            this.sceneView = new DisplaySceneView({model: this});
            this.views.push(this.sceneView);
            this.views.push(new GeomVertexMV.DisplayDOMView({model: this}));
            this.vertex.on('change', this.updateCumulativeArea, this);
        },

        destroy: function() {
            GeomVertexMV.DisplayModel.prototype.destroy.call(this);
            this.vertex.off('change', this.updateCumulativeArea, this);
        },

    });

    var DisplaySceneView = GeomVertexMV.DisplaySceneView.extend(SceneViewMixin).extend({

        initialize: function() {
            GeomVertexMV.DisplaySceneView.prototype.initialize.call(this);
        },

        remove: function() {
            GeomVertexMV.DisplaySceneView.prototype.remove.call(this);
        },

    })

    // ---------- Module ----------

    return {
        EditingModel: EditingModel,
        DisplayModel: DisplayModel,
    }

});