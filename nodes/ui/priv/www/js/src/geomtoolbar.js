define([
    'src/calculations',
    'src/toolbar', 
    'src/geometrygraphsingleton', 
    'src/commandstack', 
    'src/interactioncoordinator',
    'src/selection',
    'src/workplaneMV',
    'src/vertexMV',
    ], 
    function(
        calc,
        toolbar, 
        geometryGraph, 
        commandStack, 
        coordinator, 
        selection,
        Workplane,
        VertexMV) {

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
    });

    var Polyline = toolbar.ItemModel.extend({
        name: 'polyline',   
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

        click: function() {
            if (this.get('enabled')) {
                var selectedId = selection.selected[0];
                toolbar.ItemModel.prototype.click.call(this);
                var polyline = geometryGraph.vertexById(selectedId);
                geometryGraph.createExtrudePrototype(polyline, 1);
            }
        },

    });

    var selectItemModel   = new SelectItemModel();
    var pointItemModel    = new PointItemModel();
    var polylineItemModel = new Polyline();
    var extrudeItemModel  = new Extrude();

    var GeomToolbarModel = toolbar.Model.extend({

        initialize: function(attributes) {
            toolbar.Model.prototype.initialize.call(this, attributes);
            commandStack.on('beforePop', this.beforePop, this);
            VertexMV.eventProxy.on('committedCreate', this.committedCreate, this);
            VertexMV.eventProxy.on('cancelledCreate', this.setToSelect, this);
        },

        activate: function(item) {
            if (item !== this.activeItem) {
                this.deactivateActiveItem();
                this.setActive(item);
                this.launchTool(item);
            }
        },

        setActive: function(item) {
            this.activeItem = item;
            item.activate()
        },

        deactivateActiveItem: function() {
            if (this.activeItem) {
                this.activeItem.deactivate();
            }
        },

        launchTool: function(item) {
            VertexMV.cancelIfEditing();
            if (item.name === 'point') {
                var workplane = calc.copyObj(Workplane.getCurrent().vertex.workplane);
                geometryGraph.createPointPrototype({workplane: workplane});
            }
            if (item.name === 'polyline') {
                var workplane = calc.copyObj(Workplane.getCurrent().vertex.workplane);
                geometryGraph.createPolylinePrototype({workplane: workplane});
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
                    this.launchTool(pointItemModel);
                } else if (isPolylineCreate) {
                    this.launchTool(polylineItemModel);
                } else {
                    this.setToSelect();
                }
            }

        },

        setToSelect: function() {
            this.activate(selectItemModel);
        },

        isSelectActive: function() {
            return this.activeItem === selectItemModel;
        },

    });

    var toolbarModel = new GeomToolbarModel({name: 'geometry'});
    toolbarModel.addItem(selectItemModel);
    toolbarModel.addItem(pointItemModel);
    toolbarModel.addItem(polylineItemModel);
    toolbarModel.addItem(extrudeItemModel);
    toolbarModel.setToSelect();
    return toolbarModel;

});