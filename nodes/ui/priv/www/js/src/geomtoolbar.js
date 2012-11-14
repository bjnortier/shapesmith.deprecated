define([
    'src/toolbar', 
    'src/geometrygraphsingleton', 
    'src/commandstack', 
    'src/interactioncoordinator',
    'src/selection'
    ], 
    function(
        toolbar, 
        geometryGraph, 
        commandStack, 
        coordinator, 
        selectionManager) {

    var SelectItemModel = toolbar.ItemModel.extend({
        
        name: 'select',

        activate: function() {
            toolbar.ItemModel.prototype.activate.call(this);
            selectionManager.canSelect = true;
        },

        deactivate: function() {
            toolbar.ItemModel.prototype.deactivate.call(this);
            selectionManager.canSelect = false;
            selectionManager.deselectAll();
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
            selectionManager.on('selected', this.selectionChanged, this);
            selectionManager.on('deselected', this.selectionChanged, this);
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
                var selectedId = selectionManager.selected[0];
                toolbar.ItemModel.prototype.click.call(this);
                geometryGraph.createExtrudePrototype(selectedId);
            }
        },


    });


    var GeomToolbarModel = toolbar.Model.extend({

        initialize: function(attributes) {
            toolbar.Model.prototype.initialize.call(this, attributes);
            geometryGraph.on('committed', this.geometryCommitted, this);
            commandStack.on('beforePop', this.beforePop, this);
            coordinator.on('keydown', this.keydown, this);
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
            this.toolRelaunchVertexId = undefined;
            geometryGraph.cancelIfEditing();
            if (item.name === 'point') {
                this.toolRelaunchVertexId = geometryGraph.createPointPrototype().id;
            }
            if (item.name === 'polyline') {
                this.toolRelaunchVertexId = geometryGraph.createPolylinePrototype().id;
            }
        },

        keydown: function() {
            if (event.keyCode === 13) {
                // ???
            } else if (event.keyCode === 27) {
                // Activate the select tool
                this.activate(this.items[0]);
            }
        },

        itemClicked: function(item) {
            this.activate(item);
        },

        // If the vertex committed is the tool vertex, create another
        // of the same type. Child vertices (e.g. points of polylines)
        // can be committed, so only create another for the top-level tools 
        geometryCommitted: function(vertices) {
            if ((vertices.length === 1) && (vertices[0].id === this.toolRelaunchVertexId)) {
                this.launchTool(this.activeItem);
            } else {
                this.setToSelect();
            }
        },

        // Cancel any active tools when geometry is popped
        beforePop: function() {
            this.setToSelect();
        },

        setToSelect: function() {
            this.activate(this.items[0]);
        },

        isSelectActive: function() {
            return this.activeItem === this.items[0];
        },

    });

    var toolbarModel = new GeomToolbarModel({name: 'geometry'});
    toolbarModel.addItem(new SelectItemModel({toolbarModel: toolbarModel}));
    toolbarModel.addItem(new PointItemModel({toolbarModel: toolbarModel}));
    toolbarModel.addItem(new Polyline({toolbarModel: toolbarModel}));
    toolbarModel.addItem(new Extrude({toolbarModel: toolbarModel}));
    toolbarModel.setToSelect();
    return toolbarModel;

});