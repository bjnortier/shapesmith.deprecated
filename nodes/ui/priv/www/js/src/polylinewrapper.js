define(['src/calculations', 'src/geometrygraphsingleton', 'src/vertexwrapper', 'src/scene', 'src/workplane'], 
    function(calc, geometryGraph, vertexWrapper, sceneModel, workplaneModel) {

    // ---------- Common ----------

    var LineSceneView = {

        render: function() {
            vertexWrapper.EditingSceneView.prototype.render.call(this);
            var ambient = this.highlightAmbient || this.selectedAmbient || this.ambient || 0x333333;
            var color = this.highlightColor || this.selectedColor || this.color || 0x00dd00;
            
            var materials = [
                new THREE.MeshLambertMaterial( { ambient: ambient,  side: THREE.DoubleSide} ),
                new THREE.MeshBasicMaterial( { color: color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide } ),
            ];
            var pointChildren = geometryGraph.childrenOf(this.model.vertex);
            var coordinates = pointChildren.map(function(point) {
                return point.parameters.coordinate;
            });

            for(var i = 1; i < coordinates.length; ++i) {
                var from = calc.objToVector(coordinates[i-1]);
                var to = calc.objToVector(coordinates[i]);
                var geometry = new THREE.Geometry();
                geometry.vertices.push(from);
                geometry.vertices.push(to);
                var material = new THREE.LineBasicMaterial({ color: color });
                var line = new THREE.Line(geometry, material);
                this.sceneObject.add(line);
            }
        },

    };

    // ---------- Editing ----------

    var EditingModel = vertexWrapper.EditingModel.extend({

        initialize: function(vertex) {
            vertexWrapper.EditingModel.prototype.initialize.call(this, vertex);
            this.inClickAddsPointPhase = true;
            this.views = this.views.concat([
                new EditingDOMView({model: this}),
                new EditingLineSceneView({model: this}),
                new EditingPointAnchorSceneView({model: this, index: 0}),
            ]);
        },

        workplanePositionChanged: function(position) {
            if ((this.stage !== undefined) && (this.inClickAddsPointPhase)) {
                this.lastPosition = position;
                var point = geometryGraph.childrenOf(this.vertex)[this.stage];
                point.parameters.coordinate = {
                    x: position.x,
                    y: position.y,
                    z: position.z,
                }
                this.trigger('parametersChanged');
            }
        },

        sceneViewClick: function(viewAndEvent) {
            if (this.stage !== undefined) {
                if (viewAndEvent.view.model.vertex.type === 'point') {
                    this.vertex.parameters[this.stage] = {
                        point: viewAndEvent.view.model.vertex.id
                    }
                }
                if (this.inClickAddsPointPhase) {
                    this.stage = this.stage + 1;
                    this.addCoordinate(this.lastPosition);
                } else {
                    this.stage = undefined;
                }
                this.trigger('stageChanged', this.stage);
            }
        },

        workplaneClick: function(position) {
            if (this.stage !== undefined) {
                var point1 = geometryGraph.childrenOf(this.vertex)[this.stage];
                point1.parameters.coordinate = {
                    x : position.x,
                    y : position.y,
                    z : position.z,
                }
                if (this.inClickAddsPointPhase) {
                    this.stage = this.stage + 1;
                    this.addCoordinate(position);
                } else {
                    this.stage = undefined;
                }
                this.trigger('stageChanged', this.stage);
            }
        },

        keydown: function(event) {
            if (event.keyCode === 13) {
                if (this.inClickAddsPointPhase) {
                    geometryGraph.removeLastPointFromPolyline(this.vertex);
                    this.views[this.views.length - 1].remove();
                    this.views.splice(this.views.length - 1, 1);
                    this.stage = undefined;
                    this.inClickAddsPointPhase = false;
                    this.trigger('stageChanged', this.stage);
                } else {
                    this.ok();
                }
            } else if (event.keyCode === 27) {
                // Esc
                this.cancel();
            }
        },

        canComplete: function() {
            return geometryGraph.childrenOf(this.vertex).length > 1;
        },

        ok: function() {
            if (this.stage !== undefined) {
                geometryGraph.removeLastPointFromPolyline(this.vertex);
            }
            vertexWrapper.EditingModel.prototype.ok.call(this);
        },

        addCoordinate: function(position) {
            var point = geometryGraph.addPointToPolyline(this.vertex);
            point.parameters.coordinate = {
                x : position.x,
                y : position.y,
                z : position.z,
            }
            this.views.push(new EditingPointAnchorSceneView({model: this, index: this.stage}));
        },

    });

    var EditingDOMView = vertexWrapper.EditingDOMView.extend({

        render: function() {
            var template = 
                '<td>' +
                '<div class="title"><img src="/ui/images/icons/line32x32.png"/>' +
                '<div class="id">{{id}}</div>' + 
                '<span class="okcancel">' + 
                '<span class="ok button disabled"><img src="/ui/images/icons/ok24x24.png"/></span>' +
                '<span class="cancel button"><img src="/ui/images/icons/cancel24x24.png"/></span>' +
                '</span>' + 
                '</div>' + 
                '<div class="coordinates">' + 
                '{{{renderedCoordinates}}}' +
                '</div>' + 
                '</td>';
            var view = {
                id: this.model.vertex.id,
                renderedCoordinates: this.renderCoordinates()
            };
            this.$el.html($.mustache(template, view));
            return this;
        },

        renderCoordinates: function() {
            var template = 
                '{{#coordinates}}' +
                '{{#id}}<div class="point">{{id}}</div>{{/id}}' + 
                '{{#id}}<div class="coordinate {{i}} {{#editing}}editing{{/editing}}">' +
                '<span class="x">{{x}}</span><span class="y">{{y}}</span><span class="z">{{z}}</span>' +
                '</div>{{/id}}' +
                '{{/coordinates}}';
            var that = this;

            var pointChildren = geometryGraph.childrenOf(this.model.vertex);
            var coordinates = pointChildren.map(function(pointChild, i) {
                return {
                    editing: that.model.stage === i,
                    id: pointChild.id,
                    x: pointChild.parameters.coordinate.x, 
                    y: pointChild.parameters.coordinate.y, 
                    z: pointChild.parameters.coordinate.z, 
                    i: i,
                }
            });
            var view = {
                coordinates: coordinates
            }
            return $.mustache(template, view);
        },

        stageChanged: function(stage) {
            if (stage > 1) {
                this.$el.find('.ok').removeClass('disabled')
            }
            this.$el.find('.coordinates').html(this.renderCoordinates());
        },

        updateParams: function() {
            var index = this.model.stage, that = this;
            var point = geometryGraph.childrenOf(this.model.vertex)[index];
            ['x', 'y', 'z'].forEach(function(dim) {
                that.$el.find('.coordinate.' + index).find('.' + dim).text(
                    point.parameters.coordinate[dim]);
            });
        },
    }); 

    var EditingLineSceneView = vertexWrapper.EditingSceneView.extend(LineSceneView, {});

    var EditingPointAnchorSceneView = vertexWrapper.EditingSceneView.extend({

        initialize: function(options) {
            this.index = options.index;
            this.point = geometryGraph.childrenOf(this.model.vertex)[this.index];

            this.draggable = false; 
            vertexWrapper.EditingSceneView.prototype.initialize.call(this);
            this.on('drag', this.drag, this);
            this.on('dragEnded', this.dragEnded, this);
        },

        remove: function() {
            vertexWrapper.EditingSceneView.prototype.remove.call(this);
            this.off('drag', this.drag, this);
            this.off('dragEnded', this.dragEnded, this);
        },

        render: function() {
            vertexWrapper.EditingSceneView.prototype.render.call(this);

            var ambient = this.highlightAmbient || this.selectedAmbient || this.ambient || 0x333333;
            var color = this.highlightColor || this.selectedColor || this.color || 0x00dd00;
            var point = THREE.SceneUtils.createMultiMaterialObject(
                new THREE.CubeGeometry(1, 1, 1, 1, 1, 1), 
                [
                    new THREE.MeshBasicMaterial({color: color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                    new THREE.MeshBasicMaterial({color: color, wireframe: true, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                ]);
            point.position = calc.objToVector(this.point.parameters.coordinate);
            this.sceneObject.add(point);
        },

        drag: function(event) {
            this.dragging = true;
            this.model.stage = this.index;
            var positionOnWorkplane = calc.positionOnWorkplane(
                event, workplaneModel.node, sceneModel.view.camera);
            this.point.parameters.coordinate = {
                x: positionOnWorkplane.x,
                y: positionOnWorkplane.y,
                z: positionOnWorkplane.z,
            }
            this.model.trigger('parametersChanged');
        },

        dragEnded: function() {
            if (this.dragging) {
                this.model.stage = undefined;
                this.dragging = false;
            }
        },

    });

    // ---------- Display ----------

    var DisplayModel = vertexWrapper.DisplayModel.extend({

        initialize: function(vertex) {
            vertexWrapper.DisplayModel.prototype.initialize.call(this, vertex);
            this.views = this.views.concat([
                new DisplayLineSceneView({model: this}),
                new DisplayDOMView({model: this}),
            ]);
        },

    });

    var DisplayLineSceneView = vertexWrapper.DisplaySceneView.extend(LineSceneView);

    var DisplayDOMView = vertexWrapper.DisplayDOMView.extend({

        render: function() {
            this.$el.html('<td class="vertex display"><img src="/ui/images/icons/line32x32.png"/></td>');            return this;
        },

    })

    // ---------- Init ----------

    geometryGraph.on('vertexAdded', function(vertex) {
        if (vertex.type === 'polyline') {
            if (geometryGraph.parentsOf(vertex).length === 0) {
                if (vertex.editing) {
                    new EditingModel(vertex);
                } else {
                    new DisplayModel(vertex);
                }
            }
        }
    });

});

