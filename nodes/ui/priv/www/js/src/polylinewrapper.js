define([
    'src/calculations',
    'src/geometrygraphsingleton',
    'src/geomvertexwrapper',
    'src/scene',
    'src/workplane'], 
    function(calc, geometryGraph, geomVertexWrapper, sceneModel, workplaneModel) {

    // ---------- Common ----------

    var LineSceneView = {

        render: function() {
            geomVertexWrapper.EditingSceneView.prototype.render.call(this);
            var ambient = this.highlightAmbient || this.selectedAmbient || this.ambient || 0x333333;
            var color = this.highlightColor || this.selectedColor || this.color || 0x00dd00;
            
            var pointChildren = geometryGraph.childrenOf(this.model.vertex);
            var coordinates = pointChildren.map(function(point) {
                return point.parameters.coordinate;
            });

            var positions = [calc.objToVector(coordinates[0])];

            for(var i = 1; i < coordinates.length; ++i) {
                var from = calc.objToVector(coordinates[i-1]);
                var to = calc.objToVector(coordinates[i]);
                var geometry = new THREE.Geometry();
                geometry.vertices.push(from);
                geometry.vertices.push(to);
                var material = new THREE.LineBasicMaterial({ color: color });
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
            this.views = this.views.concat([
                new EditingDOMView({model: this}),
                new EditingLineSceneView({model: this}),
            ]);
            this.vertex.on('beforeImplicitChildCommit', this.beforeImplicitChildCommit, this);
        },

        destroy: function() {
            geomVertexWrapper.EditingModel.prototype.destroy.call(this);
            this.vertex.off('beforeImplicitChildCommit', this.beforeImplicitChildCommit, this);
        },

        beforeImplicitChildCommit: function(childVertex) {
            if (this.vertex.proto) {
                if (_.last(geometryGraph.childrenOf(this.vertex)) === childVertex) {
                    geometryGraph.addPointToPolyline(this.vertex);
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
            if (this.vertex.proto) {
                var children = geometryGraph.childrenOf(this.vertex);
                if (viewAndEvent.view.model.vertex.type === 'point') {
                    geometryGraph.removeLastPointFromPolyline(this.vertex);
                    geometryGraph.addPointToPolyline(this.vertex, viewAndEvent.view.model.vertex);
                    geometryGraph.addPointToPolyline(this.vertex);
                    this.vertex.trigger('change', this.vertex);
                } 
            }
        },

        sceneViewDblClick: function(viewAndEvent) {
            if (viewAndEvent.view.model.vertex.type === 'point') {
                geometryGraph.removeLastPointFromPolyline(this.vertex);
                this.okCreate();
            }
        },

    });

    var EditingDOMView = geomVertexWrapper.EditingDOMView.extend({

        render: function() {
            var template = 
                '<td class="vertex {{name}} editing">' + 
                '<div class="title"><img src="/ui/images/icons/line32x32.png"/>' +
                '<div class="name">{{name}}</div>' + 
                '</div>' + 
                '<div class="points">' + 
                '{{{renderedCoordinates}}}' +
                '</div>' + 
                '</td>';
            var view = {
                name: this.model.vertex.name,
                renderedCoordinates: this.renderCoordinates()
            };
            this.$el.html($.mustache(template, view));
            return this;
        },

        renderCoordinates: function() {
            // Note the coordinate class that has a number - valid class names
            // cannot start with a number, so prefix an underscore
            var template = 
                '{{#points}}' +
                '<div class="point _{{i}}">' +
                '<div class="named">{{id}}</div>' + 
                '</div>' +
                '{{/points}}';
            var that = this;

            var pointChildren = geometryGraph.childrenOf(this.model.vertex);
            var points = pointChildren.map(function(pointChild, i) {
                return {
                    editing: that.model.get('stage') === i,
                    id: pointChild.id,
                    name: pointChild.name,
                    i: i
                }
            });
            var view = {
                points: points
            }
            return $.mustache(template, view);
        },

        update: function() {
            var coordinates = this.renderCoordinates();
            this.$el.find('.points').html(coordinates);
        },

    }); 

    var EditingLineSceneView = geomVertexWrapper.EditingSceneView.extend(LineSceneView, {});


    // ---------- Display ----------

    var DisplayModel = geomVertexWrapper.DisplayModel.extend({

        initialize: function(vertex) {
            geomVertexWrapper.DisplayModel.prototype.initialize.call(this, vertex);
            this.views = this.views.concat([
                new DisplayLineSceneView({model: this}),
                new DisplayDOMView({model: this}),
            ]);
        },

        destroy: function() {
            geomVertexWrapper.DisplayModel.prototype.destroy.call(this);
        },

    });

    var DisplayLineSceneView = geomVertexWrapper.DisplaySceneView.extend(LineSceneView);

    var DisplayDOMView = geomVertexWrapper.DisplayDOMView.extend({

        render: function() {
            var view = {
                name: this.model.vertex.name,
            }
            var template = 
                '<td class="vertex {{name}} display">' +
                '<img src="/ui/images/icons/line32x32.png"/>' +
                '<div class="name">{{name}}</div>' + 
                '</td>';
            this.$el.html($.mustache(template, view));
            return this;
        },

    })

    return {
        DisplayModel: DisplayModel,
        EditingModel: EditingModel,
    }


});

