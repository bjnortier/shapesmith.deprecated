define(['src/toolbar', 'src/geometrygraphsingleton', 'src/interactioncoordinator'], 
    function(toolbar, geometryGraph, coordinator) {

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
            if (item.name === 'select') {
                geometryGraph.cancelIfEditing();
            }
            if (item.name === 'point') {
                geometryGraph.createPointPrototype();
            }
            if (item.name === 'polyline') {
                geometryGraph.createPolylinePrototype();
            }
        },

        keydown: function() {
            if (event.keyCode === 13) {
                // ???
            } else if (event.keyCode === 27) {
                this.activate(this.items[0]);
            }
        },

        itemClicked: function(item) {
            this.activate(item);
        },

        geometryCommitted: function() {
            this.launchTool(this.activeItem);
        },

    });

    var toolbarModel = new GeomToolbarModel({name: 'geometry'});
    toolbarModel.addItem(new SelectItemModel({toolbarModel: toolbarModel}));
    toolbarModel.addItem(new PointItemModel({toolbarModel: toolbarModel}));
    toolbarModel.addItem(new LineItemModel({toolbarModel: toolbarModel}));
    toolbarModel.activate(toolbarModel.items[0]);
    return toolbarModel;

});