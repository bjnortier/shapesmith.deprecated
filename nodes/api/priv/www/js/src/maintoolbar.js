define([
        'jquery',
        'lib/jquery.getQueryParam',
        'src/toolbar',
        'src/scene',
        'src/geometrygraphsingleton',
        'src/vertexmodelmanager',
        'src/interactioncoordinator',
        'src/settings',
        'src/vertexMV',
        'src/workplaneMV',
        'src/asyncAPI',
        'src/hintview',
        'requirejsplugins/text!../../images/icons/cog.svg',
        'requirejsplugins/text!../../images/icons/tag.svg',
        'requirejsplugins/text!../../images/icons/list.svg',
    ], 
    function(
            $, __$,
            toolbar,
            sceneModel,
            geometryGraph,
            vertexModelManager,
            coordinator,
            settings,
            VertexMV,
            WorkplaneMV,
            AsyncAPI,
            hintView,
            cogIcon,
            tagIcon,
            listIcon) {

    var SettingsItemModel = toolbar.ItemModel.extend({

        name: 'settings',

        click: function() {
            settings.edit();
        },

        icon: cogIcon,

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
                url: '/_api/' + globals.username + '/' + globals.design + '/refs/heads/master/',
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

        icon: tagIcon,

    });

    var ExportOBJItemModel = toolbar.ItemModel.extend({
        name: 'obj',

        click: function() {
            var graphSHA = $.getQueryParam("commit");
            window.location = '/' + globals.username + '/' + globals.design + '/obj/' + graphSHA + '/';
        },

    });

    var ExitItemModel = toolbar.ItemModel.extend({

        name: 'designs',

        click: function() {
            window.location = '/_ui/' + globals.username + '/designs.html';
        },

        icon: listIcon,

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
