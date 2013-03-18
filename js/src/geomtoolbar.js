define([
    'src/calculations',
    'src/toolbar', 
    'src/calculations',
    'src/geomnode',
    'src/geometrygraphsingleton', 
    'src/commandstack', 
    'src/interactioncoordinator',
    'src/selection',
    'src/workplaneMV',
    'src/vertexMV',
    'requirejsplugins/text!/ui/images/icons/point.svg',
    'requirejsplugins/text!/ui/images/icons/polyline.svg',
    'requirejsplugins/text!/ui/images/icons/cube.svg',
    'requirejsplugins/text!/ui/images/icons/sphere.svg',
    'requirejsplugins/text!/ui/images/icons/extrude.svg',
    'requirejsplugins/text!/ui/images/icons/subtract.svg',
    ], 
    function(
        calc,
        toolbar, 
        Calc,
        GeomNode,
        geometryGraph, 
        commandStack, 
        coordinator, 
        selection,
        WorkplaneMV,
        VertexMV,
        pointIcon,
        polylineIcon,
        cubeIcon, 
        sphereIcon,
        extrudeIcon,
        subtractIcon) {

    var SelectItemModel = toolbar.ItemModel.extend({
        
        name: 'select',

        activate: function() {
            toolbar.ItemModel.prototype.activate.call(this);
            selection.canSelect = true;
        },

        deactivate: function() {
            toolbar.ItemModel.prototype.deactivate.call(this);
            selection.canSelect = false;
            selection.deselectAll();
        },

    });

    var PointItemModel = toolbar.ItemModel.extend({

        name: 'point',
        
        activate: function() {
            toolbar.ItemModel.prototype.activate.call(this);
            var workplane = calc.copyObj(WorkplaneMV.getCurrent().vertex.workplane);
            geometryGraph.createPointPrototype({workplane: workplane});
        },

        icon: pointIcon,

    });

    var Polyline = toolbar.ItemModel.extend({
        
        name: 'polyline',   
        
        activate: function() {
            toolbar.ItemModel.prototype.activate.call(this);
            var workplane = calc.copyObj(WorkplaneMV.getCurrent().vertex.workplane);
            geometryGraph.createPolylinePrototype({workplane: workplane});
        },

        icon: polylineIcon,

    });

    var Extrude = toolbar.ItemModel.extend({
        
        name: 'extrude',

        initialize: function(attributes) {
            toolbar.ItemModel.prototype.initialize.call(this, attributes);
            this.set('enabled', false);
            selection.on('selected', this.selectionChanged, this);
            selection.on('deselected', this.selectionChanged, this);
        },

        selectionChanged: function(_, selection) {
            var polyline;
            if (selection.length === 1) {
                var vertex = geometryGraph.vertexById(selection[0]);
                if (vertex.type === 'polyline') {
                    polyline = vertex;
                }
            }
            if (polyline) {
                this.set('enabled', true);
            } else {
                this.set('enabled', false);
            }
        },

        activate: function(savedSelection) {
            toolbar.ItemModel.prototype.activate.call(this);
            if (this.get('enabled')) {
                var selectedId = savedSelection[0];
                var height = 10;

                // Extrude up to the next elevation plane 
                // if possible
                var polyline = geometryGraph.vertexById(selectedId);
                var polylineWorkplaneZ = (polyline.workplane && polyline.workplane.origin.z) || 0;

                var elevationPlanes = geometryGraph.verticesByType('elevation_plane');
                var elevationPlanesHeights = elevationPlanes.map(function(plane) {
                    return plane.parameters.height;
                }).sort();

                for (var i = 0; i < elevationPlanesHeights.length; ++i) {
                    if (elevationPlanesHeights[i] > polylineWorkplaneZ) {
                        height = elevationPlanesHeights[i] - polylineWorkplaneZ;
                        break;
                    }
                }
                geometryGraph.createExtrudePrototype(polyline, height);

            }
        },

        icon: extrudeIcon,

    });

    var Cube = toolbar.ItemModel.extend({
        
        name: 'cube',   
        
        activate: function() {
            toolbar.ItemModel.prototype.activate.call(this);

            var workplane = Calc.copyObj(WorkplaneMV.getCurrent().vertex.workplane);
            var point = new GeomNode.Point({
                editing   : true,
                proto     : true,
                implicit  : true, 
                workplane : workplane,
            });
            geometryGraph.add(point);
            
            var cubeOptions = {
                editing      : true,
                proto        : true,
                workplane    : workplane,
            }
            var cubeVertex = new GeomNode.Cube(cubeOptions);
            geometryGraph.add(cubeVertex, function() {
                geometryGraph.addEdge(cubeVertex, point);
            });
        },

        icon: cubeIcon,

    });

    var Sphere = toolbar.ItemModel.extend({
        
        name: 'sphere',   
        
        activate: function() {
            toolbar.ItemModel.prototype.activate.call(this);

            var workplane = Calc.copyObj(WorkplaneMV.getCurrent().vertex.workplane);
            var point = new GeomNode.Point({
                editing   : true,
                proto     : true,
                implicit  : true, 
                workplane : workplane,
            });
            geometryGraph.add(point);
            
            var sphereOptions = {
                editing      : true,
                proto        : true,
                workplane    : workplane,
            }
            var sphereVertex = new GeomNode.Sphere(sphereOptions);
            geometryGraph.add(sphereVertex, function() {
                geometryGraph.addEdge(sphereVertex, point);
            });
        },

        icon: sphereIcon,

    });

    var Subtract = toolbar.ItemModel.extend({
        
        name: 'subtract',

        initialize: function(attributes) {
            toolbar.ItemModel.prototype.initialize.call(this, attributes);
            this.set('enabled', false);
            selection.on('selected', this.selectionChanged, this);
            selection.on('deselected', this.selectionChanged, this);
        },

        selectionChanged: function(_, selection) {
            var polyline;
            this.set('enabled', (selection.length === 2));
        },

        activate: function(savedSelection) {
            toolbar.ItemModel.prototype.activate.call(this);
            if (this.get('enabled')) {
                var a = geometryGraph.vertexById(savedSelection[1]);
                var b = geometryGraph.vertexById(savedSelection[0]);

                var boolVertex = new GeomNode.Subtract({
                    proto: true,
                    editing: true,
                });
                geometryGraph.add(boolVertex, function() {
                    geometryGraph.addEdge(boolVertex, a);
                    geometryGraph.addEdge(boolVertex, b);
                });

            }
        },

        icon: subtractIcon,

    });


    var selectItemModel   = new SelectItemModel();
    var pointItemModel    = new PointItemModel();
    var polylineItemModel = new Polyline();
    var cubeItemModel     = new Cube();
    var sphereItemModel   = new Sphere();
    var extrudeItemModel  = new Extrude();
    var subtractItemModel = new Subtract();

    var GeomToolbarModel = toolbar.Model.extend({

        appendSelector: '#toolbar',

        initialize: function(attributes) {
            toolbar.Model.prototype.initialize.call(this, attributes);
            commandStack.on('beforePop', this.beforePop, this);
            VertexMV.eventProxy.on('committedCreate', this.committedCreate, this);
            VertexMV.eventProxy.on('cancelledCreate', this.setToSelect, this);
        },

        activate: function(item) {
            if (item !== this.activeItem) {
                var savedSelection = selection.getSelected();
                this.deactivateActiveItem();

                // Don't re-enter on cancel
                VertexMV.eventProxy.off('cancelledCreate', this.setToSelect, this);
                VertexMV.cancelIfEditing();
                VertexMV.eventProxy.on('cancelledCreate', this.setToSelect, this);

                this.setActive(item, savedSelection);
            }
        },

        setActive: function(item, savedSelection) {
            this.activeItem = item;
            item.activate(savedSelection)
        },

        deactivateActiveItem: function() {
            if (this.activeItem) {
                this.activeItem.deactivate();
            }
        },

        itemClicked: function(item) {
            this.activate(item);
        },

        // Cancel any active tools when geometry is popped
        beforePop: function() {
            this.setToSelect();
            selection.deselectAll();
        },

        committedCreate: function(committedVertices) {
            // The first vertex in the list is by convention the parent vertex 
            if (!committedVertices[0].implicit) {
                var isPointCreate = committedVertices[0].type === 'point';
                var isPolylineCreate = committedVertices[0].type === 'polyline';

                if (isPointCreate) {
                    this.setActive(pointItemModel, []);
                } else if (isPolylineCreate) {
                    this.setActive(polylineItemModel, []);
                } else {
                    this.setToSelect();
                }
            }

        },

        setToSelect: function() {
            this.activate(selectItemModel);
        },

    });

    var toolbarModel = new GeomToolbarModel({name: 'geometry'});
    var expander = new toolbar.ExpanderItem();
    toolbarModel.addItem(selectItemModel);
    toolbarModel.addItem(pointItemModel);
    toolbarModel.addItem(polylineItemModel);
    toolbarModel.addItem(cubeItemModel);
    toolbarModel.addItem(sphereItemModel);
    toolbarModel.addItem(extrudeItemModel);
    toolbarModel.addItem(subtractItemModel);
    toolbarModel.setToSelect();
    expander.toggle();
    return toolbarModel;

});