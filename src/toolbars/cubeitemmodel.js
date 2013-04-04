define([
        'calculations',
        'modelviews/workplaneMV',
        'geometrygraphsingleton',
        'icons',
        'toolbars/toolbar',
        'geomnode',
    ], function(
        Calc,
        WorkplaneMV,
        geometryGraph,
        icons,
        toolbar,
        GeomNode) {

    var Model = toolbar.ItemModel.extend({
        
        name: 'cube',   
        
        activate: function() {
            toolbar.ItemModel.prototype.activate.call(this);

            var workplane = Calc.copyObj(WorkplaneMV.getCurrent().vertex.workplane);
            var point = new GeomNode.Point({
                editing   : true,
                proto     : true,
                implicit  : true, 
                workplane : workplane,
            });
            geometryGraph.add(point);
            
            var cubeOptions = {
                editing      : true,
                proto        : true,
                workplane    : workplane,
            }
            var cubeVertex = new GeomNode.Cube(cubeOptions);
            geometryGraph.add(cubeVertex, function() {
                geometryGraph.addEdge(cubeVertex, point);
            });
        },

        icon: icons['cube'],

        createAnother: function(type) {
            return type === 'cube';
        } 

    });

    return Model;

});