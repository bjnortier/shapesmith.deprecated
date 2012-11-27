define([
    'src/calculations',
    'src/toolbar', 
    'src/geometrygraphsingleton', 
    'src/commandstack', 
    'src/interactioncoordinator',
    'src/selection',
    'src/workplaneMV',
    ], 
    function(
        calc,
        toolbar, 
        geometryGraph, 
        commandStack, 
        coordinator, 
        selection,
        Workplane) {

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


    var GeomToolbarModel = toolbar.Model.extend({

        initialize: function(attributes) {
            toolbar.Model.prototype.initialize.call(this, attributes);
            geometryGraph.on('vertexRemoved', this.vertexRemoved, this);
            commandStack.on('beforePop', this.beforePop, this);
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
            if (item.name === 'point') {
                var workplane = calc.copyObj(Workplane.getCurrent().vertex.workplane);
                geometryGraph.createPointPrototype({workplane: workplane});
            }
            if (item.name === 'polyline') {
                var workplane = calc.copyObj(Workplane.getCurrent().vertex.workplane);
                geometryGraph.createPolylinePrototype({workplane: workplane});
            }
        },

        vertexRemoved: function(vertex) {
            if (vertex.proto) {
                this.setToSelect();
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