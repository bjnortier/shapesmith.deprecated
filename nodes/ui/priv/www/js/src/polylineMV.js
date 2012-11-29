define([
        'src/calculations', 
        'src/colors',
        'src/scene', 
        'src/scenevieweventgenerator',
        'src/geometrygraphsingleton',
        'src/vertexMV',
        'src/geomvertexMV', 
        'src/pointMV', 
        'src/workplaneMV',
        'src/asyncAPI',
    ], 
    function(
        calc, 
        colors, 
        sceneModel, 
        sceneViewEventGenerator,
        geometryGraph,
        VertexMV,
        GeomVertexMV,
        PointMV,
        WorkplaneMV,
        AsyncAPI) {

    // ---------- Common ----------

    var LineSceneView = {

        render: function() {
            GeomVertexMV.EditingSceneView.prototype.render.call(this);
            var ambient = this.highlightAmbient || this.selectedAmbient || this.ambient || colors.geometry.defaultAmbient;
            var color = this.highlightColor || this.selectedColor || this.color || colors.geometry.default;
            
            var pointChildren = geometryGraph.childrenOf(this.model.vertex);
            if (pointChildren.length === 0) {
                return;
            }
            var coordinates = pointChildren.map(function(point) {
                return point.parameters.coordinate;
            });

            var positions = [calc.objToVector(coordinates[0], geometryGraph)];

            for(var i = 1; i < coordinates.length; ++i) {
                var from = calc.objToVector(coordinates[i-1], geometryGraph);
                var to = calc.objToVector(coordinates[i], geometryGraph);
                var geometry = new THREE.Geometry();
                geometry.vertices.push(from);
                geometry.vertices.push(to);
                var material = new THREE.LineBasicMaterial({ color: color, linewidth: 2 });
                var line = new THREE.Line(geometry, material);
                this.sceneObject.add(line);

                positions.push(to);
            }

            var pipe = new THREE.Mesh(
                new THREE.PipeGeometry(0.4, positions),
                new THREE.MeshBasicMaterial({ color: color, side: THREE.DoubleSide }));
            this.hiddenSelectionObject.add(pipe);
        },

    };

    // ---------- Editing ----------

    var EditingModel = GeomVertexMV.EditingModel.extend({

        initialize: function(options) {
            this.displayModelConstructor = DisplayModel;
            GeomVertexMV.EditingModel.prototype.initialize.call(this, options);

            this.domView = new EditingDOMView({model: this});
            this.views.push(this.domView);
            this.views.push(new EditingLineSceneView({model: this}));

            // Create the child models
            if (this.vertex.proto) {
                // Prototype polylines will always have an implicit point as first child
                var pointChildren = geometryGraph.childrenOf(this.vertex);
                this.subModels = [
                    new PointMV.EditingModel({
                        vertex: pointChildren[0],
                        parentModel: this
                    })
                ];
                this.activePoint = this.subModels[0];
            } else {
                this.originalImplicitChildren = _.uniq(
                    geometryGraph.childrenOf(this.vertex).filter(
                        function(v) {
                            return v.implicit;
                        }));
                this.editingImplicitChildren = this.originalImplicitChildren.map(function(child) {
                    return AsyncAPI.edit(child);
                })
                var that = this;
                this.subModels = this.originalImplicitChildren.map(function(child, i) {
                    if (child.implicit) {
                        // Replace the original model with an editing model
                        var modelToDestroy = VertexMV.getModelForVertex(child)
                        modelToDestroy.destroy();
                        return new modelToDestroy.editingModelConstructor({
                            original: child,
                            vertex: that.editingImplicitChildren[i],
                            parentModel: that
                        });
                    } 
                });
            }
        },

        workplanePositionChanged: function(position) {
            if (this.vertex.proto && this.activePoint) {
                this.activePoint.vertex.parameters.coordinate.x = position.x;
                this.activePoint.vertex.parameters.coordinate.y = position.y;
                this.activePoint.vertex.parameters.coordinate.z = position.z;
                this.activePoint.vertex.trigger('change', this.activePoint.vertex);            
            }
        },

        workplaneClick: function(position) {
            if (this.vertex.proto) {
                this.addPoint(position);
            } else {
                this.tryCommit();
            }
        },

        workplaneDblClick: function(event) {
            if (this.vertex.proto) {
                var children = geometryGraph.childrenOf(this.vertex);
                if (children.length > 2) {
                    this.removeLastPoint();
                    this.tryCommit();
                } 
            }
        },

        sceneViewClick: function(viewAndEvent) {
            if (this.vertex.proto) {
                var type = viewAndEvent.view.model.vertex.type;
                if ((type === 'point') || (type === 'implicit_point')) {

                    this.removeLastPoint();
                    var clickedPoint = viewAndEvent.view.model.vertex;
                    geometryGraph.addPointToPolyline(this.vertex, clickedPoint);
                    
                    // Finish on the first point
                    var children = geometryGraph.childrenOf(this.vertex);
                    if ((clickedPoint === _.first(children)) && (children.length > 2)) {
                        this.tryCommit();
                        this.creating = true; // Prevent double create on double click
                    } else {
                        this.addPoint(clickedPoint.parameters.coordinate);
                    }
                } else {
                    this.workplaneClick(WorkplaneMV.getCurrent().lastPosition);
                }
            }
        },

        sceneViewDblClick: function(viewAndEvent) {
            // Double-click on a point and the first click hasn't already created
            if (!this.creating && (viewAndEvent.view.model.vertex.type === 'point')) {
                geometryGraph.removeLastPointFromPolyline(this.vertex);
                this.tryCommit();
            }
        },

        addPoint: function(position) {
            var point = geometryGraph.addPointToPolyline(this.vertex);
            var newLength = this.subModels.push(new PointMV.EditingModel({
                vertex: point, 
                parentModel: this
            }));
            this.activePoint = this.subModels[newLength - 1];
            this.workplanePositionChanged(position);
        },

        removeLastPoint: function() {
            geometryGraph.removeLastPointFromPolyline(this.vertex);
            this.subModels[this.subModels.length - 1].destroy();
        },

        isChildClickable: function(childModel) {
            // Can't click the active point
            return childModel !== this.activePoint;
        },

    });

    var EditingDOMView = GeomVertexMV.EditingDOMView.extend({

        initialize: function() {
            GeomVertexMV.EditingDOMView.prototype.initialize.call(this);
        },

        remove: function() {
            GeomVertexMV.EditingDOMView.prototype.remove.call(this);
        },

        render: function() {
            var template = 
                '<td>' +
                '<table><tr>' +
                '<td class="title">' + 
                '<img src="/ui/images/icons/polyline32x32.png"/>' +
                '<div class="name">{{name}}</div>' + 
                '<div class="delete"></div>' + 
                '</td></tr><tr><td>' +
                '</div>' + 
                '<div class="points">' + 
                '</div>' + 
                '</td></tr></table>' +
                '</td>'
            var view = {
                name: this.model.vertex.name,
            };
            this.$el.html($.mustache(template, view));
            return this;
        },

        insertChild: function(childModel, childElement) {
            this.$el.find('.points').append(childElement);
        },

    }); 

    var EditingLineSceneView = GeomVertexMV.EditingSceneView.extend(LineSceneView, {});

    // ---------- Display ----------

    var DisplayModel = GeomVertexMV.DisplayModel.extend({

        initialize: function(options) {
            this.editingModelConstructor = EditingModel;
            this.displayModelConstructor = DisplayModel;
            GeomVertexMV.DisplayModel.prototype.initialize.call(this, options);

            this.views = this.views.concat([
                new DisplayLineSceneView({model: this}),
                new GeomVertexMV.DisplayDOMView({model: this}),
            ]);
        },
    });

    var DisplayLineSceneView = GeomVertexMV.DisplaySceneView.extend(LineSceneView);  


    // ---------- Module ----------

    return {
        EditingModel: EditingModel,
        DisplayModel: DisplayModel,
    }

});