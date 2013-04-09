define([
        'colors',
        'calculations', 
        'interactioncoordinator', 
        'scene',
        'settings',
        'geomnode',
        'geometrygraphsingleton',
        'modelviews/vertexMV',
        'workplanecoordinator',
        'selection',
        'asyncAPI',
    ], function(
        colors,
        calc, 
        coordinator, 
        sceneModel,
        settings,
        geomNode,
        geometryGraph,
        VertexMV,
        workplaneCoordinator,
        selection,
        AsyncAPI) {

    var currentDisplayModel = undefined;

    var EditingModel = VertexMV.EditingModel.extend({

        initialize: function(options) {
            this.SceneView = GridView;
            VertexMV.DisplayModel.prototype.initialize.call(this, options);
        },
        
    });

    var DisplayModel = VertexMV.DisplayModel.extend({

        initialize: function(options) {
            this.displayModelConstructor = DisplayModel;
            this.SceneView = GridView;
            VertexMV.DisplayModel.prototype.initialize.call(this, options);

            currentDisplayModel = this;
            this.renderAtZero = true;

            settings.on('change:gridsize', this.gridSizeChanged, this);
            coordinator.on('mousemove', this.mousemove, this);
            coordinator.on('sceneClick', this.workplaneClick, this);
            coordinator.on('sceneDblClick', this.workplaneDblClick, this);
            
            geometryGraph.on('vertexAdded', this.vertexAdded, this);
            geometryGraph.on('vertexRemoved', this.vertexRemoved, this);
            geometryGraph.on('vertexReplaced', this.vertexReplaced, this);
        },

        destroy: function() {
            VertexMV.DisplayModel.prototype.destroy.call(this);
            settings.off('change:gridsize', this.gridSizeChanged, this);
            coordinator.off('mousemove', this.mousemove, this);
            coordinator.off('sceneClick', this.workplaneClick, this);
            coordinator.off('sceneDblClick', this.workplaneDblClick, this);
            geometryGraph.off('vertexAdded', this.vertexAdded, this);
            geometryGraph.off('vertexRemoved', this.vertexRemoved, this);
            geometryGraph.off('vertexReplaced', this.vertexReplaced, this);
        },

        workplaneClick: function(event) {
            VertexMV.eventProxy.trigger('workplaneClick');
        },

        workplaneDblClick: function(eventst) {
            VertexMV.eventProxy.trigger('workplaneDblClick');
        },

        mousemove: function(event) {
            VertexMV.eventProxy.trigger('workplanePositionChanged', event);
        },

        edit: function() {
            geometryGraph.edit(this.vertex);
        },

        pushVertex: function(vertex) {
            this.persistentHeight = this.vertex.workplane.origin.z;
            this.vertex.workplane.origin.z = vertex.workplane.origin.z;
        },

        popVertex: function(vertex) {
            if (this.persistentHeight !== undefined) {
                this.vertex.workplane.origin.z = this.persistentHeight;
                this.persistentHeight = undefined;
            }
        },

        vertexAdded: function(vertex) {
            this.trigger('change');
        },

        vertexRemoved: function(vertex) {
            this.trigger('change');
        },

        // Push and pop the editing node's workplane
        // when it's being editied
        vertexReplaced: function(original, replacement) {
            if (replacement.category !== 'geometry') {
                this.trigger('change');
                return;
            }
            if (!replacement.proto && replacement.editing && !replacement.implicit) {
                this.pushVertex(replacement);
            } else if(!original.proto && original.editing && !original.implicit) {
                this.popVertex(replacement);
            }
            this.trigger('change');
        },

        gridSizeChanged: function() {
            this.trigger('change');
        },

    });

    var GridView = VertexMV.SceneView.extend({

        initialize: function() {
            VertexMV.SceneView.prototype.initialize.call(this);
            this.model.on('change', this.render, this);
            workplaneCoordinator.on('change', this.render, this);
        },

        remove: function() {
            VertexMV.SceneView.prototype.remove.call(this);
            this.model.off('change', this.render, this);
            workplaneCoordinator.off('change', this.render, this);
        },
        
        render: function() {
            VertexMV.SceneView.prototype.render.call(this);

            var grid = settings.get('gridsize');
            var boundary = Math.floor(grid*10)*10;

            var majorGridLineGeometry = new THREE.Geometry();
            var majorMaterialInside = new THREE.LineBasicMaterial({
                opacity: 0.5, 
                color: colors.workplane.majorGridLine});
            majorGridLineGeometry.vertices.push(new THREE.Vector3(-Math.floor(boundary/grid)*grid, 0, 0));
            majorGridLineGeometry.vertices.push(new THREE.Vector3(Math.ceil(boundary/grid)*grid, 0, 0));


            for (var x = -Math.floor(boundary/grid); x <= Math.ceil(boundary/grid); ++x) {
                if (x % 10 === 0) {
                    var line = new THREE.Line(majorGridLineGeometry, majorMaterialInside);
                    line.position.x = x*grid;
                    line.position.z = this.model.vertex.workplane.origin.z;
                    line.rotation.z = 90 * Math.PI / 180;
                    this.sceneObject.add(line);
                }
            }

            for (var y = -Math.floor(boundary/grid); y <= Math.ceil(boundary/grid); ++y) {
                if (y % 10 === 0) {
                    var line = new THREE.Line(majorGridLineGeometry, majorMaterialInside);
                    line.position.y = y*grid;
                    line.position.z = this.model.vertex.workplane.origin.z;
                    this.sceneObject.add(line);
                }
            }
            
            if (geometryGraph.isEditing()) {
                var minorGridLineGeometry = new THREE.Geometry();
                var minorMaterialInside = new THREE.LineBasicMaterial({ 
                    color: colors.workplane.minorGridLine, 
                    opacity: 0.1, 
                    transparent: true });

                minorGridLineGeometry.vertices.push(new THREE.Vector3(-Math.floor(boundary/grid)*grid, 0, 0));
                minorGridLineGeometry.vertices.push(new THREE.Vector3(Math.ceil(boundary/grid)*grid, 0, 0));

                for (var x = -Math.floor(boundary/grid); x <= Math.ceil(boundary/grid); ++x) {
                    if (x % 10 !== 0) {
                        var line = new THREE.Line(minorGridLineGeometry, minorMaterialInside);
                        line.position.x = x*grid;
                        line.position.z = this.model.vertex.workplane.origin.z;
                        line.rotation.z = 90 * Math.PI / 180;
                        this.sceneObject.add(line);
                    }
                }

                for (var y = -Math.floor(boundary/grid); y <= Math.ceil(boundary/grid); ++y) {
                    if (y % 10 !== 0) {
                        var line = new THREE.Line(minorGridLineGeometry, minorMaterialInside);
                        line.position.y = y*grid;
                        line.position.z = this.model.vertex.workplane.origin.z;
                        this.sceneObject.add(line);
                    }
                }
            }

            // Don't render the workplane plane if someone else rendered a zero plane
            // var shouldRenderPlane = 
            //     this.model.vertex.workplane.origin.z === 0 ?
            //         workplaneCoordinator.shouldRenderAtZero() : true;
            // if (shouldRenderPlane) {
            //     var planeGeometry = new THREE.PlaneGeometry(boundary*2, boundary*2);
            //     var planeMaterial = new THREE.MeshBasicMaterial({color: 0xffffff, transparent: true, opacity: 0.5, side: THREE.DoubleSide});
            //     var plane = new THREE.Mesh(planeGeometry, planeMaterial);
            //     plane.receiveShadow = true; 
            //     plane.position.z = this.model.vertex.workplane.origin.z - 0.05;
            //     this.sceneObject.add(plane);
            // }

        },

    });

    return {
        EditingModel: EditingModel,
        DisplayModel: DisplayModel,
        getCurrent  : function() { return currentDisplayModel; },
    }

});