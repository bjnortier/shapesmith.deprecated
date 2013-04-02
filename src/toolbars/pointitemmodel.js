define([
        'calculations',
        'workplaneMV',
        'geometrygraphsingleton',
        'icons',
        'toolbars/toolbar'
    ], function(
        Calc,
        WorkplaneMV,
        geometryGraph,
        icons,
        toolbar) {

    var Model = toolbar.ItemModel.extend({

        name: 'point',
        
        activate: function() {
            toolbar.ItemModel.prototype.activate.call(this);
            var workplane = Calc.copyObj(WorkplaneMV.getCurrent().vertex.workplane);
            geometryGraph.createPointPrototype({workplane: workplane});
        },

        icon: icons['point'],

        createAnother: function(type) {
            return type === 'point';
        } 

    });

    return Model;
})