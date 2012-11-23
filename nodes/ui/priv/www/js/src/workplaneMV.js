define([
        'src/colors',
        'src/calculations', 
        'src/interactioncoordinator', 
        'src/scene',
        'src/geomnode',
        'src/geometrygraphsingleton',
        'src/vertexwrapper',
    ], function(
        colors,
        calc, 
        coordinator, 
        sceneModel,
        geomNode,
        geometryGraph,
        vertexWrapper) {

    var currentDisplayModel = undefined;

    var DisplayModel = Backbone.Model.extend({

        initialize: function(vertex) {
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
        },

        destroy: function() {
            this.gridView.remove();
            this.settingsView.remove();
        },

        workplaneClick: function(event) {
            this.trigger('click', this.lastPosition);
        },

        workplaneDblClick: function(event) {
            this.trigger('dblclick', this.lastPosition);
        },

        mousemove: function(event) {
            var positionOnWorkplane = calc.positionOnWorkplane(event, this.vertex, this.camera);
            if (!this.lastPosition || !positionOnWorkplane.equals(this.lastPosition)) {
                this.lastPosition = positionOnWorkplane.addSelf(calc.objToVector(this.vertex.workplane.origin));
                this.sceneView.updateScene = true;
                this.trigger('positionChanged', this.lastPosition);
            }
        },

        edit: function() {
            geometryGraph.edit(this.vertex);
        },

    });

    var EditingModel = Backbone.Model.extend({

        initialize: function(vertex) {
            currentDisplayModel = this;
            this.vertex = vertex;
            this.sceneView = sceneModel.view;
            this.scene = this.sceneView.scene;
            this.camera = this.sceneView.camera;
            this.gridView = new GridView({model: this});
            this.settingsView = new SettingsEditingView({model: this});
        },

        destroy: function() {
            this.gridView.remove();
            this.settingsView.remove();
        },

    });


    var SettingsDisplayView = vertexWrapper.DisplayDOMView.extend({

        tagName: 'table',

        initialize: function() {
            vertexWrapper.DisplayDOMView.prototype.initialize.call(this);
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
            this.model.edit();
        },

    });

    var SettingsEditingView = vertexWrapper.EditingDOMView.extend({

        tagName: 'table',
        className: 'vertex',

        initialize: function() {
            vertexWrapper.EditingDOMView.prototype.initialize.call(this);
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

    });    

    var GridView = vertexWrapper.SceneView.extend({


        render: function() {
            vertexWrapper.SceneView.prototype.render.call(this);

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