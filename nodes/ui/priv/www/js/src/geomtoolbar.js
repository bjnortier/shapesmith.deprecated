define(['src/toolbar'], function(toolbar) {


    var VertexModel = toolbar.ItemModel.extend({

        icon: 'vertex32x32.png',
        name: 'vertex',

    });

    return {
        create: function() {
            var toolbarModel = new toolbar.Model({name: 'geometry'});
            toolbarModel.addItem(new VertexModel());
        }
    };

});