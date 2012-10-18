define(['src/toolbar'], function(toolbar) {


    var PointModel = toolbar.ItemModel.extend({
        icon: 'point32x32.png',
        name: 'point',
    });

    var LineModel = toolbar.ItemModel.extend({
        icon: 'line32x32.png',
        name: 'line',
    });



    var toolbarModel = new toolbar.Model({name: 'geometry'});
    toolbarModel.addItem(new PointModel());
    toolbarModel.addItem(new LineModel());
    return toolbarModel;

});