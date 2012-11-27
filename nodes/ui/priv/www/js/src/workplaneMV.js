define([
        'src/colors',
        'src/calculations', 
        'src/interactioncoordinator', 
        'src/scene',
        'src/geomnode',
        'src/geometrygraphsingleton',
        'src/vertexMV',
        'src/selection',
    ], function(
        colors,
        calc, 
        coordinator, 
        sceneModel,
        geomNode,
        geometryGraph,
        VertexMV,
        selection) {

    var currentDisplayModel = undefined;

    var DisplayModel = VertexMV.DisplayModel.extend({

        initialize: function(vertex) {
            this.editingModelConstructor = EditingModel;
            this.displayModelConstructor = DisplayModel;

            VertexMV.DisplayModel.prototype.initialize.call(this, vertex);

            currentDisplayModel = this;
            this.vertex = vertex;

            coordinator.on('mousemove', this.mousemove, this);
            coordinator.on('sceneClick', this.workplaneClick, this);
            coordinator.on('sceneDblClick', this.workplaneDblClick, this);
            this.sceneView = sceneModel.view;
            this.scene = this.sceneView.scene;
            this.camera = this.sceneView.camera;
            
            this.gridView = new GridView({model: this});
            this.settingsView = new SettingsDisplayView({model: this});
            this.persistentHeight = 0;
            geometryGraph.on('vertexReplaced', this.vertexReplaced, this);
        },

        destroy: function() {
            VertexMV.DisplayModel.prototype.destroy.call(this);

            this.gridView.remove();
            this.settingsView.remove();

            geometryGraph.off('vertexReplaced', this.vertexReplaced, this);
        },

        workplaneClick: function(event) {
            this.trigger('click', this.lastPosition);
        },

        workplaneDblClick: function(eventst) {
            this.trigger('dblclick', this.lastPosition);
        },

        mousemove: function(event) {
            var positionOnWorkplane = calc.positionOnWorkplane(event, this.vertex, this.camera);
            if (!this.lastPosition || !positionOnWorkplane.equals(this.lastPosition)) {
                this.lastPosition = positionOnWorkplane.addSelf(calc.objToVector(this.vertex.workplane.origin));
                if (this.lastPosition) {
                    this.sceneView.updateScene = true;
                    this.trigger('positionChanged', this.lastPosition);
                }
            }
        },

        edit: function() {
            geometryGraph.edit(this.vertex);
        },

        pushVertex: function(vertex) {
            this.persistentHeight = this.vertex.workplane.origin.z;
            this.vertex.workplane.origin.z = vertex.workplane.origin.z;
            this.trigger('change');
        },

        popVertex: function(vertex) {
            this.vertex.workplane.origin.z = this.persistentHeight;
            this.trigger('change');
        },

        vertexReplaced: function(original, replacement) {
            if (replacement.type === 'workplane') {
                return;
            }
            if (replacement.editing && !replacement.implicit) {
                this.pushVertex(replacement);
            } else  if (original.editing && !original.implciit) {
                this.popVertex(replacement);
            }
        },

    });

    var EditingModel = VertexMV.EditingModel.extend({

        initialize: function(original, vertex) {
            this.displayModelConstructor = DisplayModel;
            VertexMV.EditingModel.prototype.initialize.call(this, original, vertex);
            
            currentDisplayModel = this;
            this.vertex = vertex;
            this.sceneView = sceneModel.view;
            this.scene = this.sceneView.scene;
            this.camera = this.sceneView.camera;
            this.gridView = new GridView({model: this});
            this.settingsView = new SettingsEditingView({model: this});
        },

        destroy: function() {
            VertexMV.EditingModel.prototype.destroy.call(this);
            this.gridView.remove();
            this.settingsView.remove();
        },

    });


    var SettingsDisplayView = VertexMV.DisplayDOMView.extend({

        tagName: 'table',

        initialize: function() {
            VertexMV.DisplayDOMView.prototype.initialize.call(this);
            $('#workplane-settings').append(this.$el);
        },

        render: function() {
            var view = {
                settings: [{
                        label: 'snap',
                        value: this.model.vertex.parameters.snap
                }]
            };
            var template = '{{#settings}}<tr><td>{{label}}</td><td>{{value}}</td></tr>{{/settings}}';
            this.$el.html($.mustache(template, view));
            return this;
        },

        events: {
            'click' : 'click',
        },

        click: function() {
            selection.selectOnly(this.model.vertex.id);
        },

    });

    var SettingsEditingView = VertexMV.EditingDOMView.extend({

        tagName: 'table',
        className: 'vertex editing',

        initialize: function() {
            VertexMV.EditingDOMView.prototype.initialize.call(this);
            $('#workplane-settings').append(this.$el);
        },

        render: function() {
            var view = {
                settings: [
                    {
                        label: 'snap',
                        value: this.model.vertex.parameters.snap
                    }   
                ]
            };
            var template = '{{#settings}}<tr><td>{{label}}</td><td><input class="field snap" type="text" value="{{value}}"></input></td></tr>{{/settings}}';
            this.$el.html($.mustache(template, view));
            return this;
        },

        updateFromDOM: function() {
            this.model.vertex.parameters.snap = this.$el.find('.snap').val();
        },

        update: function() {
            if (this.model.vertex.errors) {
                this.$el.addClass('error');
            } else {
                this.$el.removeClass('error');
            }
        },


    });    

    var GridView = VertexMV.SceneView.extend({

        initialize: function() {
            VertexMV.SceneView.prototype.initialize.call(this);
            this.model.on('change', this.render, this);
        },

        remove: function() {
            VertexMV.SceneView.prototype.remove.call(this);
            this.model.off('change', this.render, this);
        },
        
        render: function() {
            VertexMV.SceneView.prototype.render.call(this);

            var majorGridLineGeometry = new THREE.Geometry();
            var minorGridLineGeometry = new THREE.Geometry();
            var majorMaterialInside = new THREE.LineBasicMaterial({ 
                color: colors.workplane.majorGridLine, 
                opacity: 0.5, 
                transparent: false });
            var minorMaterialInside = new THREE.LineBasicMaterial({ 
                color: colors.workplane.minorGridLine, 
                opacity: 0.1, 
                transparent: true });

            var snap = this.model.vertex.parameters.snap;

            var boundary = Math.floor(snap*10)*10;

            majorGridLineGeometry.vertices.push(new THREE.Vector3(-Math.floor(boundary/snap)*snap, 0, 0));
            majorGridLineGeometry.vertices.push(new THREE.Vector3(Math.ceil(boundary/snap)*snap, 0, 0));
            minorGridLineGeometry.vertices.push(new THREE.Vector3(-Math.floor(boundary/snap)*snap, 0, 0));
            minorGridLineGeometry.vertices.push(new THREE.Vector3(Math.ceil(boundary/snap)*snap, 0, 0));

            for (var x = -Math.floor(boundary/snap); x <= Math.ceil(boundary/snap); ++x) {
                var material = (x % 10 == 0) ? majorMaterialInside : minorMaterialInside;
                var geometry = (x % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
                var line = new THREE.Line(geometry, material);
                line.position.x = x*snap;
                line.position.z = this.model.vertex.workplane.origin.z;
                line.rotation.z = 90 * Math.PI / 180;
                this.sceneObject.add(line);
            }

            for (var y = -Math.floor(boundary/snap); y <= Math.ceil(boundary/snap); ++y) {
                var material = (y % 10 == 0) ? majorMaterialInside : minorMaterialInside;
                var geometry = (y % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
                var line = new THREE.Line(geometry, material);
                line.position.y = y*snap;
                line.position.z = this.model.vertex.workplane.origin.z;
                this.sceneObject.add(line);
            }

        },

    });

    var findVertex = function() {
        var workplaneVertices = geometryGraph.filteredVertices(function(v) {
            return v.type === 'workplane';
        });
        if (workplaneVertices.length === 0) {
            console.error('No workplane found');
        } else if (workplaneVertices.length === 1) {
            this.vertex = workplaneVertices[0].workplane;
        } else {
            this.vertex = workplaneVertices[0].workplane;
            console.error('More than one workplane vertex in graph - using first');
        }
    }

    return {
        DisplayModel: DisplayModel,
        EditingModel: EditingModel,
        getCurrent  : function() { return currentDisplayModel; },
    }

});