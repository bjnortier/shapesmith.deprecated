define(['src/calculations', 'src/geometrygraphsingleton', 'src/vertexwrapper', 'src/scene', 'src/workplane'], 
    function(calc, geometryGraph, vertexWrapper, sceneModel, workplaneModel) {

    // ---------- Common ----------

    var LineSceneView = {

        render: function() {
            vertexWrapper.EditingSceneView.prototype.render.call(this);
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

            var pipe = THREE.SceneUtils.createMultiMaterialObject(
                new THREE.PipeGeometry(3, positions),
                [
                    new THREE.MeshLambertMaterial( { ambient: ambient,  side: THREE.DoubleSide} ),
                    new THREE.MeshBasicMaterial( { color: color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide } ),
                ]);
            this.hiddenSelectionObject.add(pipe);
        },

    };

    // ---------- Editing ----------

    var EditingModel = vertexWrapper.EditingModel.extend({

        initialize: function(vertex) {
            vertexWrapper.EditingModel.prototype.initialize.call(this, vertex);
            this.views = this.views.concat([
                new EditingDOMView({model: this}),
                new EditingLineSceneView({model: this}),
            ]);
            geometryGraph.on('vertexReplaced', this.vertexReplaced, this);
            this.vertex.on('descendantChanged', this.descendantChanged, this);
        },

        destroy: function() {
            vertexWrapper.EditingModel.prototype.destroy.call(this);
            geometryGraph.off('vertexReplaced', this.vertexReplaced, this);
            this.vertex.off('descendantChanged', this.descendantChanged, this);
        },

        // If the replaced vertex is the last point
        // in the polyline, add another point
        vertexReplaced: function(original, replacement) {
            if (_.last(geometryGraph.childrenOf(this.vertex)) === replacement) {
                geometryGraph.addPointToPolyline(this.vertex);
            }
        },

        // Trigger a 'change' event if any of the child polylines changed
        descendantChanged: function(descendant) {
            this.vertex.trigger('change', this.vertex);
        },

        workplaneDblClick: function(event) {
            var children = geometryGraph.childrenOf(this.vertex);
            if (children.length > 3) {
                geometryGraph.remove(children[children.length-1]);
                geometryGraph.remove(children[children.length-2]);
                this.ok();
            } 
        },

        sceneViewClick: function(viewAndEvent) {
            // if (this.get('stage') !== undefined) {
            //     if (viewAndEvent.view.model.vertex.type === 'point') {
            //         // Remove last point and it's anchor view
            //         geometryGraph.removeLastPointFromPolyline(this.vertex);
            //         this.views[this.views.length - 1].remove();
            //         this.views.splice(this.views.length - 1, 1);

            //         // Add the named point and a new anchor view
            //         geometryGraph.addPointToPolyline(this.vertex, viewAndEvent.view.model.vertex);
            //         this.views.push(new EditingPointAnchorSceneView({
            //             model: this, index: this.get('stage')
            //         }));
            //     }
            //     if (this.inClickAddsPointPhase) {
            //         this.set('stage',  this.get('stage') + 1);
            //         this.addCoordinate(this.lastPosition);
            //     } else {
            //         this.set('stage', undefined);
            //     }
            // }
        },


    });

    var EditingDOMView = vertexWrapper.EditingDOMView.extend({

        render: function() {
            var template = 
                '<td>' +
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
                '{{#name}}<div class="named">{{id}}</div>{{/name}}' + 
                '{{^name}}<div class="coordinate{{#editing}} editing{{/editing}}">' +
                '<span class="x">{{x}}</span><span class="y">{{y}}</span><span class="z">{{z}}</span>' +
                '</div>{{/name}}' +
                '</div>' +
                '{{/points}}';
            var that = this;

            var pointChildren = geometryGraph.childrenOf(this.model.vertex);
            var points = pointChildren.map(function(pointChild, i) {
                return {
                    editing: that.model.get('stage') === i,
                    id: pointChild.id,
                    name: pointChild.name,
                    x: pointChild.parameters.coordinate.x, 
                    y: pointChild.parameters.coordinate.y, 
                    z: pointChild.parameters.coordinate.z, 
                    i: i,
                }
            });
            var view = {
                points: points
            }
            return $.mustache(template, view);
        },

    }); 

    var EditingLineSceneView = vertexWrapper.EditingSceneView.extend(LineSceneView, {});


    // ---------- Display ----------

    var DisplayModel = vertexWrapper.DisplayModel.extend({

        initialize: function(vertex) {
            vertexWrapper.DisplayModel.prototype.initialize.call(this, vertex);
            this.views = this.views.concat([
                new DisplayLineSceneView({model: this}),
                new DisplayDOMView({model: this}),
            ]);
            this.vertex.on('descendantChanged', this.descendantChanged, this);
        },

        destroy: function() {
            vertexWrapper.DisplayModel.prototype.destroy.call(this);
            this.vertex.off('descendantChanged', this.descendantChanged, this);
        },

        // Trigger a 'change' event if any of the child polylines changed
        descendantChanged: function(descendant) {
            this.vertex.trigger('change', this.vertex);
        },

    });

    var DisplayLineSceneView = vertexWrapper.DisplaySceneView.extend(LineSceneView);

    var DisplayDOMView = vertexWrapper.DisplayDOMView.extend({

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

