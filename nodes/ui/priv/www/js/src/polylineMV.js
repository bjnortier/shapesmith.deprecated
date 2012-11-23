define([
    'src/calculations',
    'src/colors',
    'src/geometrygraphsingleton',
    'src/geomvertexwrapper',
    'src/pointMV',
    'src/scene',
    'src/workplane'], 
    function(calc, colors, geometryGraph, geomVertexWrapper, pointMV, sceneModel, workplaneModel) {

    // ---------- Common ----------

    var LineSceneView = {

        render: function() {
            geomVertexWrapper.EditingSceneView.prototype.render.call(this);
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

    var EditingModel = geomVertexWrapper.EditingModel.extend({

        initialize: function(vertex) {
            geomVertexWrapper.EditingModel.prototype.initialize.call(this, vertex);
            this.editingDOMView = new EditingDOMView({model: this});
            this.implicitAppendElement = this.editingDOMView.$el.find('.points');
            this.views = this.views.concat([
                this.editingDOMView,
                new EditingLineSceneView({model: this}),
            ]);
            this.vertex.on('beforeImplicitChildCommit', this.beforeImplicitChildCommit, this);
        },

        destroy: function() {
            geomVertexWrapper.EditingModel.prototype.destroy.call(this);
            this.vertex.off('beforeImplicitChildCommit', this.beforeImplicitChildCommit, this);
        },

        // A point has been added to the polyline with a click
        beforeImplicitChildCommit: function(childVertex) {
            if (this.vertex.proto) {
                if (_.last(geometryGraph.childrenOf(this.vertex)) === childVertex) {
                    var point = geometryGraph.addPointToPolyline(this.vertex);
                }
            }
        },

        workplaneDblClick: function(event) {
            this.finished = true;
            var children = geometryGraph.childrenOf(this.vertex);
            if (children.length > 2) {
                // Remove the extra point added
                geometryGraph.removeLastPointFromPolyline(this.vertex);
                this.okCreate();
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
                        this.okCreate();
                        this.creating = true; // Prevent double create on double click
                    } else {
                        geometryGraph.addPointToPolyline(this.vertex);
                        this.vertex.trigger('change', this.vertex);
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

    });

    var EditingDOMView = geomVertexWrapper.EditingDOMView.extend({

        initialize: function() {
            this.lastRenderedPointIds = [];
            geomVertexWrapper.EditingDOMView.prototype.initialize.call(this);
        },

        remove: function() {
            geomVertexWrapper.EditingDOMView.prototype.remove.call(this);
        },

        render: function() {
            var template = 
                '<td colspan="2">' + 
                '<div class="title"><img src="/ui/images/icons/polyline32x32.png"/>' +
                '<div class="name">{{name}}</div>' + 
                '</div>' + 
                '<div class="points">' + 
                '</div>' + 
                '</td>';
            var view = {
                name: this.model.vertex.name,
                // renderPoints: this.renderPoints()
            };
            this.$el.html($.mustache(template, view));
            // this.update();
            return this;
        },

        // renderPoints: function() {
        //     // Note the coordinate class that has a number - valid class names
        //     // cannot start with a number, so prefix an underscore

        //     var that = this;
        //     var pointChildren = geometryGraph.childrenOf(this.model.vertex);
        //     var appendElement = $('<div></div>');
        //     var points = pointChildren.map(function(pointVertex, i) {

        //         if (pointVertex.editing) {
        //             var modelForPoint = new pointMV.EditingModel(pointVertex, appendElement);
        //         } else {
        //             var template = 
        //                 '<div class="point _{{i}}">' +
        //                 '<div class="named">{{id}}</div>' +
        //                 '</div>';
        //             appendElement.append($.mustache(template, pointVertex));
        //         }

        //     });

        //     return appendElement;
        // },

        // update: function() {
        //     // Re-render when the list of children has changed. Point field updates
        //     // are handles by the point model(s)
        //     var childIds = geometryGraph.childrenOf(this.model.vertex).map(function(v) {
        //         return v.id;
        //     });
        //     var allSame = (childIds.length === this.lastRenderedPointIds.length);
        //     for (var i = 0; allSame && (i < childIds.length); ++i) {
        //         if (childIds[i] !== this.lastRenderedPointIds[i]) {
        //             allSame = false;
        //             break;
        //         }
        //     }

        //     if (!allSame) {
        //         var pointsElement = this.renderPoints();
        //         this.$el.find('.points').empty();
        //         this.$el.find('.points').append(pointsElement);
        //         this.lastRenderedPointIds = childIds;
        //     }
        // },

    }); 

    var EditingLineSceneView = geomVertexWrapper.EditingSceneView.extend(LineSceneView, {});


    // ---------- Display ----------

    var DisplayModel = geomVertexWrapper.DisplayModel.extend({

        initialize: function(vertex) {
            geomVertexWrapper.DisplayModel.prototype.initialize.call(this, vertex);
            this.views = this.views.concat([
                new DisplayLineSceneView({model: this}),
                new geomVertexWrapper.DisplayDOMView({model: this}),
            ]);
        },

        destroy: function() {
            geomVertexWrapper.DisplayModel.prototype.destroy.call(this);
        },

    });

    var DisplayLineSceneView = geomVertexWrapper.DisplaySceneView.extend(LineSceneView);    

    return {
        DisplayModel: DisplayModel,
        EditingModel: EditingModel,
    }


});

