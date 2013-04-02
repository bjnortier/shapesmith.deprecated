define([
        'jquery',
        'lib/jquery.getQueryParam',
        'toolbars/toolbar',
        'toolbars/geomtoolbar',
        'scene',
        'geometrygraphsingleton',
        'vertexmodelmanager',
        'interactioncoordinator',
        'settings',
        'vertexMV',
        'workplaneMV',
        'asyncAPI',
        'hintview',
        'icons',
    ], 
    function(
            $, __$,
            toolbar,
            geomToolbar,
            sceneModel,
            geometryGraph,
            vertexModelManager,
            coordinator,
            settings,
            VertexMV,
            WorkplaneMV,
            AsyncAPI,
            hintView,
            icons) {

    var SettingsItemModel = toolbar.ItemModel.extend({

        name: 'settings',

        click: function() {
            settings.edit();
        },

        icon: icons['cog'],

    });

    var SaveItemModel = toolbar.ItemModel.extend({

        name: 'tag',

        click: function() {

            // sceneModel.view.renderer.autoClear = false;
            sceneModel.view.updateScene = true;
            sceneModel.view.render();
            var screenshot = sceneModel.view.renderer.domElement.toDataURL();

            var commit = $.getQueryParam("commit");
            $.ajax({
                type: 'PUT',
                url: '/_api/' + globals.user + '/' + globals.design + '/refs/heads/master/',
                contentType: 'application/json',
                data: JSON.stringify({commit: commit, screenshot: screenshot}),
                success: function(response) {
                    console.info('SAVE: ' + commit);
                    hintView.set('Saved.');
                    setTimeout(function() {
                        hintView.clear();
                    }, 1000);
                },
                error: function(jqXHR, textStatus, errorThrown) {
                    console.error('could not save');
                }
            });
        },

        icon: icons['tag'],

    });

    var ExportOBJItemModel = toolbar.ItemModel.extend({
        name: 'obj',

        click: function() {
            var graphSHA = $.getQueryParam("commit");
            window.location = '/' + globals.user + '/' + globals.design + '/obj/' + graphSHA + '/';
        },

    });

    var ExitItemModel = toolbar.ItemModel.extend({

        name: 'designs',

        click: function() {
            window.location = '/_ui/' + globals.user + '/designs';
        },

        icon: icons['list'],

    });

    var MainToolbarModel = toolbar.Model.extend({

        appendSelector: '#toolbar',

        initialize: function(attributes) {
            toolbar.Model.prototype.initialize.call(this, attributes);
            geometryGraph.on('committed', this.geometryCommitted, this);
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
    var expander = new toolbar.ExpanderItem();
    toolbarModel.addItem(new SettingsItemModel());
    toolbarModel.addItem(new SaveItemModel());
    toolbarModel.addItem(new ExitItemModel());
    toolbarModel.addItem(expander);
    toolbarModel.addItem(new ExportOBJItemModel());
    expander.toggle();
    return toolbarModel;

});
