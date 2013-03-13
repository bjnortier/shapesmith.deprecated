define([
        'jquery',
        'lib/jquery.mustache',
        'src/calculations',
        'src/worldcursor',
        'src/scene',
        'src/geometrygraphsingleton',
        'src/vertexMV',
        'src/geomvertexMV', 
        'src/pointMV', 
        'src/heightanchorview',
        'src/asyncAPI',
        'src/lathe/pool',
    ], 
    function(
        $, __$,
        calc,
        worldCursor,
        sceneModel,
        geometryGraph,
        VertexMV,
        GeomVertexMV,
        PointMV,
        EditingHeightAnchor,
        AsyncAPI,
        Lathe) {

    // ---------- Common ----------

    var SceneViewMixin = {

        render: function() {
            GeomVertexMV.SceneView.prototype.render.call(this);

            var points = geometryGraph.childrenOf(this.model.vertex);
            if (points.length !== 2) {
                return;
            }

            var materials;
            if (this.model.vertex.editing) {
                materials = [
                    this.materials.editing.face, 
                    this.materials.editing.wire
                ]
            } else {
                materials = [
                    this.materials.normal.face, 
                    this.materials.normal.wire
                ]
            }

            var positionAndDims = this.determinePositionAndDims(points);
            var position = positionAndDims.position;
            var dims = positionAndDims.dims;

            var cube = THREE.SceneUtils.createMultiMaterialObject(
                new THREE.CubeGeometry(dims.width, dims.depth, dims.height),
                materials);
            cube.position = position.add(new THREE.Vector3(
                dims.width/2, dims.depth/2, dims.height/2));
            this.sceneObject.add(cube);
        },

        determinePositionAndDims: function(points) {
            var positions = points.map(function(p) {
                return calc.objToVector(
                    p.parameters.coordinate, geometryGraph, THREE.Vector3);
            });

            var dims = {
                width : Math.abs(positions[1].x - positions[0].x),
                depth : Math.abs(positions[1].y - positions[0].y),
                height : Math.abs(geometryGraph.evaluate(this.model.vertex.parameters.height)),
            }

            var position = new THREE.Vector3(
                Math.min(positions[0].x, positions[1].x),
                Math.min(positions[0].y, positions[1].y),
                Math.min(positions[0].z, positions[0].z + dims.height));

            return {position: position, dims:dims};
        },
    }

    // ---------- Editing ----------

    var EditingModel = GeomVertexMV.EditingModel.extend({

        initialize: function(options) {
            this.displayModelConstructor = DisplayModel;
            GeomVertexMV.EditingModel.prototype.initialize.call(this, options);

            var points = geometryGraph.childrenOf(this.vertex);

            this.faceGroup = options.faceGroup;
            this.domView = new EditingDOMView({model: this});
            this.views.push(this.domView);

            // Create the child models
            var that = this;
            if (this.vertex.proto) {
                this.stage = 0;
                this.updateHint();
                this.subModels = [
                    new PointMV.EditingModel({
                        vertex: points[0],
                        parentModel: this,
                    }),
                ];
                this.activePoint = this.subModels[0];
            } else {
                this.originalImplicitChildren = geometryGraph.childrenOf(this.vertex);
                this.editingImplicitChildren = [];
                this.editingImplicitChildren = this.originalImplicitChildren.map(function(child, i) {
                    var editing = AsyncAPI.edit(child);

                    if (!that.faceGroup) {
                        that.views.push(new EditingHeightAnchor({
                            model: that, 
                            heightKey: 'height',
                            pointVertex: editing
                        }));
                    }
                    return editing;
                })
                this.subModels = this.originalImplicitChildren.map(function(child, i) {

                    // Replace the original model with an editing model
                    var modelToDestroy = VertexMV.getModelForVertex(child)
                    modelToDestroy.destroy();
                    return new modelToDestroy.editingModelConstructor({
                        original: child,
                        vertex: that.editingImplicitChildren[i],
                        parentModel: that
                    });
                });
            }

            this.setMainSceneView(new EditingSceneView({model: this}));
        },

        workplanePositionChanged: function(position, event) {
            if (this.vertex.proto) {
                if (this.activePoint) {
                    this.activePoint.vertex.parameters.coordinate.x = position.x;
                    this.activePoint.vertex.parameters.coordinate.y = position.y;
                    this.activePoint.vertex.parameters.coordinate.z = position.z;
                    this.activePoint.vertex.trigger('change', this.activePoint.vertex);            
                } else if (this.activeHeightAnchor) {
                    this.activeHeightAnchor.drag(position, undefined, event);
                }
            }
        },

        sceneViewClick: function(viewAndEvent) {
            if (this.vertex.proto) {
                this.workplaneClick(worldCursor.lastPosition);
            }
        },

        workplaneClick: function(position) {
            if (this.vertex.proto) {
                if (this.stage === 0) {
                    this.addPoint(position);
                    ++this.stage;
                    this.updateHint();
                } else if (this.stage === 1) {
                    ++this.stage;
                    this.activeHeightAnchor = new EditingHeightAnchor({
                        model: this, 
                        heightKey: 'height',
                        pointVertex: this.activePoint.vertex
                    });
                    this.activeHeightAnchor.dragStarted();
                    this.activeHeightAnchor.isDraggable = function() {
                        return false;
                    };
                    this.views.push(this.activeHeightAnchor);
                    delete this.activePoint;
                    this.updateHint();
                } else if (this.stage === 2) {
                    this.tryCommit();
                }
            } else {
                this.tryCommit();
            }
        },

        addPoint: function(position) {
            var point = geometryGraph.addPointToParent(this.vertex);
            if (this.stage === 0) {
                this.subModels.push(new PointMV.EditingModel({
                    vertex: point, 
                    parentModel: this
                }));
                this.activePoint = this.subModels[1];
            }
            this.workplanePositionChanged(position);
        },

        isChildClickable: function(childModel) {
            // Can't click the active point
            return childModel !== this.activePoint;
        },

        updateHint: function() {
            if (this.vertex.proto) {
                switch(this.stage) {
                    case 0: 
                        this.hintView.set('Click to add a corner.');
                        break;
                    case 1:
                        this.hintView.set('Click to add a corner diagonally opposite.');
                        break;
                    case 2:
                        this.hintView.set('Click to set the height.');
                        break;
                }
            }
        },


    });

    var EditingDOMView = GeomVertexMV.EditingDOMView.extend({

        render: function() {
            var template = 
                '<table><tr>' +
                '<td class="title">' + 
                '<img src="/ui/images/icons/cube24x24.png"/>' +
                '<div class="name">{{name}}</div>' + 
                '<div class="delete"></div>' + 
                '</td></tr><tr><td>' +
                '</div>' + 
                '<div>' + 
                'height <input class="field height" type="text" value="{{height}}"></input>' +
                '</div>' +
                '<div class="points">' + 
                '</div>' + 
                '</td></tr></table>';
            var view = {
                name      : this.model.vertex.name,
                height    : this.model.vertex.parameters.height,
            };
            this.$el.html($.mustache(template, view));
            return this;
        },

        insertChild: function(childModel, childElement) {
            this.$el.find('.points').append(childElement);
        },

        update: function() {
            var that = this;
            ['height'].forEach(function(key) {
                that.$el.find('.field.' + key).val(
                    that.model.vertex.parameters[key]);
            });
        },

        updateFromDOM: function() {
            var that = this;
            ['height'].forEach(function(key) {
                try {
                    var expression = that.$el.find('.field.' + key).val();
                    that.model.vertex.parameters[key] = expression;
                } catch(e) {
                    console.error(e);
                }
            });
            this.model.vertex.trigger('change', this.model.vertex);
        }

    }); 


    var EditingSceneView = GeomVertexMV.EditingSceneView.extend(SceneViewMixin).extend({

        initialize: function(options) {
            GeomVertexMV.EditingSceneView.prototype.initialize.call(this);
            this.on('dragEnded', this.dragEnded, this);
            this.on('drag', this.drag, this);
        },

        remove: function() {
            GeomVertexMV.EditingSceneView.prototype.remove.call(this);
            this.off('dragEnded', this.dragEnded, this);
            this.off('drag', this.drag, this);
        },



    });


    // ---------- Display ----------

    var DisplayModel = GeomVertexMV.DisplayModel.extend({

        initialize: function(options) {
            this.editingModelConstructor = EditingModel;
            this.displayModelConstructor = DisplayModel;
            GeomVertexMV.DisplayModel.prototype.initialize.call(this, options);
            this.sceneView = new DisplaySceneView({model: this});
            this.views.push(this.sceneView);
            this.views.push(new DisplayDOMView({model: this}));
            this.vertex.on('change', this.updateCumulativeArea, this);
        },

        destroy: function() {
            GeomVertexMV.DisplayModel.prototype.destroy.call(this);
            this.vertex.off('change', this.updateCumulativeArea, this);
        },

    });

    var DisplayDOMView = GeomVertexMV.DisplayDOMView.extend({

        render: function() {
            var parameters = this.model.vertex.parameters;
            var color = (parameters.material && parameters.material.color) || '#888888';
            var view = {
                name: this.model.vertex.name,
                type: this.model.vertex.type,
                fill: color,
                stroke: color,
            }
            var template = 
                '<div class="title" draggable="true">' + 
                '<svg xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:cc="http://creativecommons.org/ns#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:svg="http://www.w3.org/2000/svg" xmlns="http://www.w3.org/2000/svg" xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd" xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape" width="24" height="24" id="svg5562" version="1.1" inkscape:version="0.458.2 r9819" sodipodi:docname="cube.svg">  <defs id="defs5564" /><g inkscape:label="Layer 1" inkscape:groupmode="layer" id="layer1" transform="translate(0,-1028.3622)"><g id="g5036-4" style="fill:{{fill}};fill-opacity:0.45;stroke:{{stroke}};stroke-opacity:1" transform="matrix(0.35304955,0,0,0.35304955,-83.431102,917.19794)" inkscape:export-filename="/Users/bjnortier/development/shapesmith.hoenix/nodes/api/priv/www/images/icons/cube32x32.greyscale.png" inkscape:export-xdpi="43.626923" inkscape:export-ydpi="43.626923"><path inkscape:export-ydpi="43.626923" inkscape:export-xdpi="43.626923" inkscape:export-filename="/Users/bjnortier/development/shapesmith.hoenix/nodes/api/priv/www/images/icons/cube32x32.png" sodipodi:nodetypes="ccccc" inkscape:connector-curvature="0" id="path4033-9" d="m 240.452857,360.64789 33.78572,-8.67857 26.21428,15.67857 -38,14 z" style="fill:{{fill}};fill-opacity:0.45;stroke:{{stroke}};stroke-width:1px;stroke-inecap:butt;stroke-linejoin:round;stroke-opacity:1" /><path inkscape:export-ydpi="43.626923" inkscape:export-xdpi="43.626923" inkscape:export-filename="/Users/bjnortier/development/shapesmith.hoenix/nodes/api/priv/www/images/icons/cube32x32.png" sodipodi:nodetypes="ccccc" inkscape:connector-curvature="0" id="path4031-1" d="m 274.42857,316.64789 -0.21428,35.5 -33.78572,8.5 -1,-41 z" style="fill:{{fill}};fill-opacity:0.45;stroke:{{stroke}};stroke-width:1px;stroke-inecap:butt;stroke-linejoin:round;stroke-opacity:1" /><path inkscape:export-ydpi="43.626923" inkscape:export-xdpi="43.626923" inkscape:export-filename="/Users/bjnortier/development/shapesmith.hoenix/nodes/api/priv/www/images/icons/cube32x32.png" sodipodi:nodetypes="ccccc" inkscape:connector-curvature="0" id="path4029-5" d="m 274.42857,316.64789 -0.21428,35.5 26.71428,15.5 0,-45 z" style="fill:{{fill}};fill-opacity:0.45;stroke:{{stroke}};stroke-width:1px;stroke-inecap:butt;stroke-linejoin:round;stroke-opacity:1" /><path inkscape:export-ydpi="43.626923" inkscape:export-xdpi="43.626923" inkscape:export-filename="/Users/bjnortier/development/shapesmith.hoenix/nodes/api/priv/www/images/icons/cube32x32.png" sodipodi:nodetypes="ccccc" inkscape:connector-curvature="0" id="path4025-5" d="m 262.07143,381.46932 38.5,-14 0,-45 -38.5,7 z" style="fill:{{fill}};fill-opacity:0.45;stroke:{{stroke}};stroke-width:1px;stroke-inecap:butt;stroke-linejoin:round;stroke-opacity:1" /><path inkscape:export-ydpi="43.626923" inkscape:export-xdpi="43.626923" inkscape:export-filename="/Users/bjnortier/development/shapesmith.hoenix/nodes/api/priv/www/images/icons/cube32x32.png" sodipodi:nodetypes="ccccc" inkscape:connector-curvature="0" id="path4027-5" d="m 239.07143,319.46932 35,-3 26.5,6 -38.5,7 z" style="fill:{{fill}};fill-opacity:0.45;stroke:{{stroke}};stroke-width:1px;stroke-inecap:butt;stroke-linejoin:round;stroke-opacity:1" /><path inkscape:export-ydpi="43.626923" inkscape:export-xdpi="43.626923" inkscape:export-filename="/Users/bjnortier/development/shapesmith.hoenix/nodes/api/priv/www/images/icons/cube32x32.png" sodipodi:nodetypes="ccccc" inkscape:connector-curvature="0" id="path4023-8" d="m 240.07143,360.456932 -1,-41 23,10 0,52 z" style="fill:{{fill}};fill-opacity:0.45;stroke:{{stroke}};stroke-width:1px;stroke-inecap:butt;stroke-linejoin:round;stroke-opacity:1" /></g></g></svg>' +
                '<div class="name">{{name}}</div>' + 
                '<div class="delete"></div>' +
                '</div>';
            this.$el.html($.mustache(template, view));
            return this;
        },    

    });

    var DisplaySceneView = GeomVertexMV.DisplaySceneView.extend(SceneViewMixin).extend({

        initialize: function() {
            GeomVertexMV.DisplaySceneView.prototype.initialize.call(this);
        },

        remove: function() {
            GeomVertexMV.DisplaySceneView.prototype.remove.call(this);
        },

        render: function() {
            GeomVertexMV.SceneView.prototype.render.call(this);
            var points = geometryGraph.childrenOf(this.model.vertex);
            var positionAndDims = this.determinePositionAndDims(points);

            var jobId = Lathe.createCube(
                positionAndDims.position.x,
                positionAndDims.position.y,
                positionAndDims.position.z,
                positionAndDims.dims.width,
                positionAndDims.dims.depth,
                positionAndDims.dims.height);

            var that = this;
            Lathe.broker.on(jobId, function(result) {
                that.model.vertex.bsp = result.bsp;
                var toMesh = that.polygonsToMesh(result.polygons);
                var faceGeometry = toMesh.geometry;
                var meshObject = THREE.SceneUtils.createMultiMaterialObject(faceGeometry, [
                    that.materials.normal.face, 
                ]);
                that.sceneObject.add(meshObject);
                sceneModel.view.updateScene = true;
            });
        },

    })



    // ---------- Module ----------

    return {
        EditingModel: EditingModel,
        DisplayModel: DisplayModel,
    }

});