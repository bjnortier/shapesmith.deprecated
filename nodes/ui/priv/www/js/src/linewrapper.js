define(['src/calculations', 'src/geometrygraph', 'src/vertexwrapper'], function(calc, geometryGraph, vertexWrapper) {

    // ---------- Common ----------

    var SceneView = {

        render: function() {
            vertexWrapper.EditingSceneView.prototype.render.call(this);

            var materials = [
                new THREE.MeshLambertMaterial( { ambient: this.ambientColor || 0x333333,  side: THREE.DoubleSide} ),
                new THREE.MeshBasicMaterial( { color: this.color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide } ),
            ];
            var vertex = this.model.vertex;
            for(var i = 0; i < vertex.parameters.length; ++i) {
                var coordinate = vertex.parameters[i];
                var point = THREE.SceneUtils.createMultiMaterialObject(
                    new THREE.SphereGeometry(0.25, 10, 10), materials);
                point.position = calc.objToVector(coordinate);
                this.sceneObject.add(point);
            }
            for(var i = 1; i < vertex.parameters.length; ++i) {
                var from = calc.objToVector(vertex.parameters[i-1]);
                var to = calc.objToVector(vertex.parameters[i]);
                var geometry = new THREE.Geometry();
                geometry.vertices.push(from);
                geometry.vertices.push(to);
                var material = new THREE.LineBasicMaterial({ color: this.color });
                var line = new THREE.Line(geometry, material);
                this.sceneObject.add(line);
            }
        },

    };

    // ---------- Editing ----------

    var EditingModel = vertexWrapper.EditingModel.extend({

        initialize: function(vertex) {
            vertexWrapper.EditingModel.prototype.initialize.call(this, vertex);
            this.views = this.views.concat([
                new EditingDOMView({model: this}),
                new EditingSceneView({model: this}),
            ]);
        },

        workplanePositionChanged: function(position) {
            this.lastPosition = position;
            this.vertex.parameters[this.stage].x = position.x;
            this.vertex.parameters[this.stage].y = position.y;
            this.vertex.parameters[this.stage].z = position.z;
            this.trigger('parametersChanged');
        },

        windowClick: function() {
            this.stage = this.stage + 1;
            var newPointPosition = this.lastPosition || {x:0, y:0, z:0};
            this.vertex.parameters[this.stage] = newPointPosition;
            this.trigger('stageChanged', this.stage);
        },

        canComplete: function() {
            return this.stage > 1;
        },

        ok: function() {
            this.vertex.parameters.splice(this.stage, 1);
            vertexWrapper.EditingModel.prototype.ok.call(this);
        }

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
                '<div class="coordinate {{i}}">' +
                '<span class="x">{{x}}</span><span class="y">{{y}}</span><span class="z">{{z}}</span>' +
                '</div>' +
                '{{/coordinates}}';
            var coordinates = this.model.vertex.parameters.map(function(parameter, i) {
                    return {
                        x: parameter.x, 
                        y: parameter.y, 
                        z: parameter.z, 
                        i: i
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

    var EditingSceneView = vertexWrapper.EditingSceneView.extend(SceneView, {

    });

    // ---------- Display ----------

    var DisplayModel = vertexWrapper.DisplayModel.extend({

        initialize: function(vertex) {
            vertexWrapper.DisplayModel.prototype.initialize.call(this, vertex);
            this.views = this.views.concat([
                new DisplaySceneView({model: this}),
                new DisplayDOMView({model: this}),
            ]);
        },

    });

    var DisplaySceneView = vertexWrapper.DisplaySceneView.extend(SceneView);

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

