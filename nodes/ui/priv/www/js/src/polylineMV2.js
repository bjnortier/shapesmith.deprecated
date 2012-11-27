define([
        'src/calculations', 
        'src/colors',
        'src/scene', 
        'src/scenevieweventgenerator',
        'src/geometrygraphsingleton',
        'src/geomvertexMV', 
        'src/implicitpointMV',
        'src/asyncAPI',
    ], 
    function(
        calc, 
        colors, 
        sceneModel, 
        sceneViewEventGenerator,
        geometryGraph,
        GeomVertexMV,
        ImplicitPointMV,
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

        initialize: function(original, vertex) {
            this.displayModelConstructor = DisplayModel;
            GeomVertexMV.EditingModel.prototype.initialize.call(this, original, vertex);

            this.domView = new EditingDOMView({model: this});
            this.views.push(this.domView);
            this.views.push(new EditingLineSceneView({model: this}));

            // Create the child models
            var that = this;
            var pointChildren = geometryGraph.childrenOf(this.vertex);
            this.subModels = pointChildren.map(function(child) {
                if (child.implicit) {
                    return new ImplicitPointMV.EditingModel(undefined, child, that);
                } else {
                    throw Error('not implemented');
                }
            });

            if (this.vertex.proto) {
                this.activePoint = this.subModels[0];
            }
        },

        destroy: function() {
            GeomVertexMV.EditingModel.prototype.destroy.call(this);
            this.subModels.forEach(function(model) {
                model.destroy();
            });
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
            var that = this;
            if(this.vertex.proto) {
                this.addPoint(position);
            } 
        },

        workplaneDblClick: function(event) {
            var children = geometryGraph.childrenOf(this.vertex);
            if (children.length > 2) {
                // Remove the extra point added
                geometryGraph.removeLastPointFromPolyline(this.vertex);
                this.tryCommit();
            } 
        },

        sceneViewClick: function(viewAndEvent) {
            this.lastClickSource = viewAndEvent.view;

            if (this.vertex.proto) {
                var children = geometryGraph.childrenOf(this.vertex);
                if (viewAndEvent.view.model.vertex.type === 'point') {

                    geometryGraph.removeLastPointFromPolyline(this.vertex);
                    
                    // Finish on the first point
                    var clickedPoint = viewAndEvent.view.model.vertex;
                    geometryGraph.addPointToPolyline(this.vertex, clickedPoint);
                    if (clickedPoint === _.first(children)) {
                        this.tryCommit();
                        this.creating = true; // Prevent double create on double click
                    } else {
                        this.addPoint(clickedPoint.parameters.coordinate);
                    }
                } 
            }
        },

        sceneViewDblClick: function(viewAndEvent) {
            // It may happen that a dbl-click on click has created a new polyline, and
            // THIS model is the new polyline. Thus disregard a dbl-click that didn't follow
            // a click
            if (!this.lastClickSource || (this.lastClickSource.cid !== viewAndEvent.view.cid)) {
                return;
            }

            // Double-click on a point and the first click hasn't already 
            // created
            if (!this.creating && (viewAndEvent.view.model.vertex.type === 'point')) {
                geometryGraph.removeLastPointFromPolyline(this.vertex);
                this.okCreate();
            }
        },

        addPoint: function(position) {
            var point = geometryGraph.addPointToPolyline(this.vertex);
            var newLength = this.subModels.push(new ImplicitPointMV.EditingModel(this, undefined, point));
            this.activePoint = this.subModels[newLength - 1];
            this.workplanePositionChanged(position);
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
                '<img src="/ui/images/icons/point32x32.png"/>' +
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

        initialize: function(vertex, possibleEditingParentModel) {
            this.editingModelConstructor = EditingModel;
            this.displayModelConstructor = DisplayModel;
            GeomVertexMV.DisplayModel.prototype.initialize.call(this, vertex);

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