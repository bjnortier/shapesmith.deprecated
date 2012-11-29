define([
        'src/calculations',
        'src/colors',
        'src/geometrygraphsingleton',
        'src/geomvertexwrapper',
        'src/pointMV',
        'src/scene',
    ], 
    function(calc, colors, geometryGraph, geomVertexWrapper, pointMV, sceneModel) {

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
                geometryGraph.on('committedImplicit', this.committedImplicit, this);
        },

        destroy: function() {
            geomVertexWrapper.EditingModel.prototype.destroy.call(this);
            geometryGraph.off('committedImplicit', this.committedImplicit, this);
        },

        committedImplicit: function(vertices) {
            if (this.vertex.proto && (vertices.length === 1)) {
                var vertex = vertices[0];
                if (_.last(geometryGraph.childrenOf(this.vertex)) === vertex) {
                    var point = geometryGraph.addPointToPolyline(this.vertex);
                }
            }
        },

        workplaneDblClick: function(event) {
            console.log('workplaneDblClick', viewAndEvent.view.model.vertex.id);
            
            this.finished = true;
            var children = geometryGraph.childrenOf(this.vertex);
            if (children.length > 2) {
                // Remove the extra point added
                geometryGraph.removeLastPointFromPolyline(this.vertex);
                this.okCreate();
            } 
        },

        sceneViewClick: function(viewAndEvent) {
            console.log('sceneViewClick', viewAndEvent.view.model.vertex.id);
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
            console.log('sceneViewDblClick', viewAndEvent.view.model.vertex.id);

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
                '<td>' +
                '<table><tr>' +
                '<td class="title">' + 
                '<img src="/ui/images/icons/point32x32.png"/>' +
                '<div class="name">{{name}}</div>' + 
                '{{^implicit}}<div class="delete"></div>{{/implicit}}' + 
                '</td></tr><tr><td>' +
                '</div>' + 
                '<div class="points">' + 
                '</div>' + 
                '</td></tr></table>' +
                '</td>'
            var view = {
                name: this.model.vertex.name,
                // renderPoints: this.renderPoints()
            };
            this.$el.html($.mustache(template, view));
            // this.update();
            return this;
        },

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

