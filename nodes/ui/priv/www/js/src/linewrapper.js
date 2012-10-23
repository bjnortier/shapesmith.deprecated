define(['src/calculations', 'src/geometrygraph', 'src/vertexwrapper', 'src/scene', 'src/workplane'], 
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
            var coordinates = this.model.vertex.parameters.map(function(parameter) {
                if (parameter.hasOwnProperty('point')) {
                    return geometryGraph.getPointCoordinates(parameter.point);
                } else {
                    return parameter;
                }
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
                this.vertex.parameters[this.stage] = {
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
                this.vertex.parameters[this.stage] = {
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
                    this.vertex.parameters.splice(this.stage, 1);
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
            return this.vertex.parameters.length > 1;
        },

        ok: function() {
            if (this.stage !== undefined) {
                this.vertex.parameters.splice(this.stage, 1);
            }
            vertexWrapper.EditingModel.prototype.ok.call(this);
        },

        addCoordinate: function(position) {
            this.vertex.parameters[this.stage] = {
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
                '{{^id}}<div class="coordinate {{i}} {{#editing}}editing{{/editing}}">' +
                '<span class="x">{{x}}</span><span class="y">{{y}}</span><span class="z">{{z}}</span>' +
                '</div>{{/id}}' +
                '{{/coordinates}}';
            var that = this;
            var coordinates = this.model.vertex.parameters.map(function(parameter, i) {
                var common = {
                    editing: that.model.stage === i
                }
                if (parameter.hasOwnProperty('point')) {
                    return _.extend({
                        id: parameter.point
                    }, common);
                } else {
                    return _.extend({
                        x: parameter.x, 
                        y: parameter.y, 
                        z: parameter.z, 
                        i: i,
                    }, common);
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
            ['x', 'y', 'z'].forEach(function(dim) {
                that.$el.find('.coordinate.' + index).find('.' + dim).text(
                    that.model.vertex.parameters[index][dim]);
            });
        },
    }); 

    var EditingLineSceneView = vertexWrapper.EditingSceneView.extend(LineSceneView, {});

    var EditingPointAnchorSceneView = vertexWrapper.EditingSceneView.extend({

        initialize: function(options) {
            this.index = options.index;
            this.draggable = !this.model.vertex.parameters[this.index].hasOwnProperty('point');
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
            if (this.model.vertex.parameters[this.index].point) {
                return;
            }
            var ambient = this.highlightAmbient || this.selectedAmbient || this.ambient || 0x333333;
            var color = this.highlightColor || this.selectedColor || this.color || 0x00dd00;
            var point = THREE.SceneUtils.createMultiMaterialObject(
                new THREE.CubeGeometry(1, 1, 1, 1, 1, 1), 
                [
                    new THREE.MeshBasicMaterial({color: color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                    new THREE.MeshBasicMaterial({color: color, wireframe: true, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                ]);
            point.position = calc.objToVector(this.model.vertex.parameters[this.index]);
            this.sceneObject.add(point);
        },

        drag: function(event) {
            this.dragging = true;
            this.model.stage = this.index;
            var positionOnWorkplane = calc.positionOnWorkplane(
                event, workplaneModel.node, sceneModel.view.camera);
            this.model.vertex.parameters[this.index] = {
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
        if (vertex.type === 'line') {
            if (vertex.editing) {
                new EditingModel(vertex);
            } else {
                new DisplayModel(vertex);
            }
        }
    });

});

