define([
        'jquery',
        'lib/jquery.mustache',
        'src/calculations', 
        'src/colors',
        'src/scene', 
        'src/scenevieweventgenerator',
        'src/worldcursor',
        'src/geometrygraphsingleton',
        'src/vertexMV',
        'src/modelviews/geomvertexMV', 
        'src/pointMV', 
        'src/workplaneMV',
        'src/asyncAPI',
        'requirejsplugins/text!/ui/images/icons/polyline.svg',
    ], 
    function(
        $, __$,
        calc, 
        colors, 
        sceneModel, 
        sceneViewEventGenerator,
        worldCursor,
        geometryGraph,
        VertexMV,
        GeomVertexMV,
        PointMV,
        WorkplaneMV,
        AsyncAPI,
        icon) {

    // ---------- Common ----------

    var LineSceneView = {

        render: function() {
            GeomVertexMV.EditingSceneView.prototype.render.call(this);
            
            var pointChildren = geometryGraph.childrenOf(this.model.polyline);
            if (pointChildren.length === 0) {
                return;
            }
            var coordinates = pointChildren.map(function(point) {
                return point.parameters.coordinate;
            });

            var positions = [calc.objToVector(coordinates[0], geometryGraph, THREE.Vector3)];

            var material = this.model.vertex.editing ? 
                this.materials.editing.edge : this.materials.normal.edge;

            for(var i = 1; i < coordinates.length; ++i) {
                var from = calc.objToVector(coordinates[i-1], geometryGraph, THREE.Vector3);
                var to = calc.objToVector(coordinates[i], geometryGraph, THREE.Vector3);
                var geometry = new THREE.Geometry();
                geometry.vertices.push(from);
                geometry.vertices.push(to);
                var line = new THREE.Line(geometry, material);
                this.sceneObject.add(line);
                positions.push(to);
            }

            var pipe = new THREE.Mesh(
                new THREE.PipeGeometry(0.4, positions),
                new THREE.MeshBasicMaterial({color: 0x000000, side: THREE.DoubleSide }));
            this.hiddenSelectionObject.add(pipe);
        },

    };

    // ---------- Editing ----------

    var EditingModel = GeomVertexMV.EditingModel.extend({

        initialize: function(options) {
            this.displayModelConstructor = DisplayModel;
            GeomVertexMV.EditingModel.prototype.initialize.call(this, options);

            if (this.vertex.implicit) {
                return;
            }

            this.polyline = this.vertex;

            // Create the child models
            if (this.vertex.proto) {
                this.updateHint();
                // Prototype polylines will always have an implicit point as first child
                var pointChildren = geometryGraph.childrenOf(this.vertex);
                this.activePoint = pointChildren[0];
            } else {
                this.originalImplicitChildren = _.uniq(
                    geometryGraph.childrenOf(this.vertex).filter(
                        function(v) {
                            return v.implicit;
                        }));
                this.editingImplicitChildren = this.originalImplicitChildren.map(function(child) {
                    return AsyncAPI.edit(child);
                })
            }

            this.setMainSceneView(new EditingLineSceneView({model: this}));
        },

        addTreeView: function() {
            var domView = new EditingDOMView({model: this});
            this.views.push(domView);
            return domView;
        },

        workplanePositionChanged: function(position) {
            if (this.vertex.implicit) {
                return;
            }
            if (this.vertex.proto && this.activePoint) {
                this.activePoint.parameters.coordinate.x = position.x;
                this.activePoint.parameters.coordinate.y = position.y;
                this.activePoint.parameters.coordinate.z = position.z;
                this.activePoint.trigger('change', this.activePoint);            
            }
        },

        workplaneClick: function(position) {
            if (this.vertex.implicit) {
                return;
            }

            if (this.vertex.proto) {
                this.addPoint(position);
            } else {
                this.tryCommit();
            }
        },

        workplaneDblClick: function(event) {
            if (this.vertex.implicit) {
                return;
            }

            if (this.vertex.proto) {
                var children = geometryGraph.childrenOf(this.vertex);
                if (children.length > 2) {
                    this.removeLastPoint();
                    this.tryCommit();
                } 
            }
        },

        sceneViewClick: function(viewAndEvent) {
            if (this.vertex.implicit || (viewAndEvent.view.model.vertex === this.activePoint)) {
                return;
            }

            if (this.vertex.proto) {
                var type = viewAndEvent.view.model.vertex.type;
                if (type === 'point') {

                    this.removeLastPoint();
                    var clickedPoint = viewAndEvent.view.model.vertex;
                    geometryGraph.addPointToParent(this.vertex, clickedPoint);
                    
                    // Finish on the first point
                    var children = geometryGraph.childrenOf(this.vertex);
                    if ((clickedPoint === _.first(children)) && (children.length > 2)) {
                        this.tryCommit();
                        this.creating = true; // Prevent double create on double click
                    } else {
                        this.addPoint(clickedPoint.parameters.coordinate);
                    }
                } else {
                    this.workplaneClick(worldCursor.lastPosition);
                }
            }
        },

        sceneViewDblClick: function(viewAndEvent) {
            if (this.vertex.implicit) {
                return;
            }

            // Double-click on a point and the first click hasn't already created
            if (!this.creating && (viewAndEvent.view.model.vertex.type === 'point')) {
                geometryGraph.removeLastPointFromPolyline(this.vertex);
                this.tryCommit();
            }
        },

        addPoint: function(position) {
            var point = geometryGraph.addPointToParent(this.vertex);
            this.activePoint = point;
            this.workplanePositionChanged(position);
            this.updateHint();
        },

        removeLastPoint: function() {
            geometryGraph.removeLastPointFromPolyline(this.vertex);
        },

        updateHint: function() {
            if (this.vertex.proto) {
                var points = geometryGraph.childrenOf(this.polyline);
                if (points.length < 3) {
                    this.hintView.set('Click to add a corner.');
                } else if (points.length === 3) {
                    this.hintView.set('Click to add a corner. Double-click to end.');
                } else {
                    this.hintView.set('Click to add a corner. Double-click or click on first corner to end.');
                }
            }
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
                '<table><tr>' +
                '<td class="title">' + 
                '<div class="icon24">' + icon + '</div>' +
                '<div class="name">{{name}}</div>' + 
                '<div class="delete"></div>' + 
                '</td></tr><tr><td>' +
                '</div>' + 
                '<div class="children"></div>' +
                '</td></tr></table>';
            var view = {
                name: this.model.vertex.name,
            };
            this.$el.html($.mustache(template, view));
            return this;
        },

    }); 

    var EditingLineSceneView = GeomVertexMV.EditingSceneView.extend(LineSceneView, {});

    // ---------- Display ----------

    var DisplayModel = GeomVertexMV.DisplayModel.extend({

        initialize: function(options) {
            this.editingModelConstructor = EditingModel;
            this.displayModelConstructor = DisplayModel;
            GeomVertexMV.DisplayModel.prototype.initialize.call(this, options);

            this.polyline = this.vertex;
            this.sceneView = new DisplayLineSceneView({model: this});
            this.views.push(this.sceneView);
        },

        icon: icon,

    });

    var DisplayLineSceneView = GeomVertexMV.DisplaySceneView.extend(LineSceneView);


    // ---------- Module ----------

    return {
        LineSceneView : LineSceneView,
        EditingModel  : EditingModel,
        DisplayModel  : DisplayModel,
    }

});