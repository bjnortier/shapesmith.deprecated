define([
    'vertexmodelmanager',
    'modelviews/modelgraph',
    'icons',
    'modelviews/pointMV',
    'modelviews/polylineMV',
    'modelviews/cubeMV',
    'modelviews/sphereMV',
    'modelviews/subtractMV',
    'toolbars/geomtoolbar',
    'toolbars/maintoolbar',
    'toolbars/pointitemmodel',
    'toolbars/polylineitemmodel',
    'toolbars/cubeitemmodel',
    'toolbars/sphereitemmodel',
    'toolbars/subtractitemmodel',
    ], function(
        vertexModelManager,
        modelgraph,
        icons,
        PointMV,
        PolylineMV,
        CubeMV,
        SphereMV,
        SubtractMV,
        geomToolbar,
        mainToolbar,
        PointItemModel,
        PolylineItemModel,
        CubeItemModel,
        SphereItemModel,
        SubtractItemModel) {

    var init = function() {

        modelgraph.addWrapper('point', PointMV);
        modelgraph.addWrapper('polyline', PolylineMV);
        modelgraph.addWrapper('cube', CubeMV);
        modelgraph.addWrapper('sphere', SphereMV);
        modelgraph.addWrapper('subtract', SubtractMV);

        geomToolbar.addItem(new PointItemModel());
        geomToolbar.addItem(new PolylineItemModel());
        geomToolbar.addItem(new CubeItemModel());
        geomToolbar.addItem(new SphereItemModel());
        geomToolbar.addItem(new SubtractItemModel());

    }

    return {
        init: init
    }
});