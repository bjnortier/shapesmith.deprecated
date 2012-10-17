define(['src/toolbar'], function(toolbar) {


    var VertexModel = toolbar.ItemModel.extend({

        icon: 'vertex32x32.png',

        // initialize: function() {
        //     Backbone.View.prototype.initialize.call(this);
        // },

        click: function() {
            console.log('click!');
        },

    });

    var toolbarModel = new toolbar.Model({name: 'geometry'});
    toolbarModel.addItem(new VertexModel());
});