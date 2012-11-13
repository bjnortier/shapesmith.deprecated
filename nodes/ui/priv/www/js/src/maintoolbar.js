define(['src/toolbar',
        'src/geometrygraphsingleton',
        'src/vertexmodelmanager',
        'src/interactioncoordinator',
        ], 
    function(
            toolbar,
            geometryGraph,
            vertexModelManager,
            coordinator,
            bsqParser,
            bsqUtils,
            bsqToolbarItemModel) {

    var SaveItemModel = toolbar.ItemModel.extend({
        name: 'save',

        click: function() {
            var commit = $.getQueryParam("commit");
            $.ajax({
                type: 'PUT',
                url: '/' + SS.session.username + '/' + SS.session.design + '/refs/heads/master/',
                contentType: 'application/json',
                data: JSON.stringify(commit),
                success: function(response) {
                    console.info('SAVE: ' + commit);
                },
                error: function(jqXHR, textStatus, errorThrown) {
                    console.error('could not save');
                }
            });
        },

    });

    var ExitItemModel = toolbar.ItemModel.extend({
        name: 'exit',

        click: function() {
            window.location = '/' + SS.session.username + '/designs';
        },

    });

    var MainToolbarModel = toolbar.Model.extend({

        initialize: function(attributes) {
            toolbar.Model.prototype.initialize.call(this, attributes);
            geometryGraph.on('committed', this.geometryCommitted, this);
            coordinator.on('keydown', this.keydown, this);
        },

        activate: function(item) {
        },

        setActive: function(item) {
            this.activeItem = item;
            item.activate();
        },

        deactivateActiveItem: function() {
            if (this.activeItem) {
                this.activeItem.deactivate();
            }
        },

        itemClicked: function(item) {
            this.activate(item);
        },

    });

    var toolbarModel = new MainToolbarModel({name: 'main'});
    toolbarModel.addItem(new SaveItemModel({toolbarModel: toolbarModel}));
    toolbarModel.addItem(new ExitItemModel({toolbarModel: toolbarModel}));
    return toolbarModel;

});
