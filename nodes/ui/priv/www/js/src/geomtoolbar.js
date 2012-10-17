define(['src/toolbar'], function(toolbar) {


    var VertexModel = toolbar.ItemModel.extend({
        icon: 'vertex32x32.png',
        name: 'vertex',
    });

    var LineModel = toolbar.ItemModel.extend({
        icon: 'line32x32.png',
        name: 'line',
    });


    return {
        create: function() {
            var toolbarModel = new toolbar.Model({name: 'geometry'});
            toolbarModel.addItem(new VertexModel());
            toolbarModel.addItem(new LineModel());
        }
    };

});