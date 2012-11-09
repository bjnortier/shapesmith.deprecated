define(['src/toolbar', 'src/geometrygraphsingleton', 'src/commandstack', 'src/interactioncoordinator'], 
    function(toolbar, geometryGraph, commandStack, coordinator) {

    var SelectItemModel = toolbar.ItemModel.extend({
        icon: 'mouse32x32.png',
        name: 'select',
    });

    var PointItemModel = toolbar.ItemModel.extend({
        icon: 'point32x32.png',
        name: 'point',
    });

    var LineItemModel = toolbar.ItemModel.extend({
        icon: 'line32x32.png',
        name: 'polyline',
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
            this.toolVertexId = undefined;
            geometryGraph.cancelIfEditing();
            if (item.name === 'point') {
                this.toolVertexId = geometryGraph.createPointPrototype().id;
            }
            if (item.name === 'polyline') {
                this.toolVertexId = geometryGraph.createPolylinePrototype().id;
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
        geometryCommitted: function(vertex) {
            if (vertex.id === this.toolVertexId) {
                this.launchTool(this.activeItem);
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
    toolbarModel.addItem(new LineItemModel({toolbarModel: toolbarModel}));
    toolbarModel.setToSelect();
    return toolbarModel;

});