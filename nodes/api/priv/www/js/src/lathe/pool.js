define([
        'underscore',
        'lathe/primitives/cube',
        'lathe/primitives/sphere',
        'lathe/polygon3d',
        'lathe/bsp',
        'lathe/conv',
        'backbone-events',
    ], 
    function(
        _,
        Cube,
        Sphere,
        Polygon3D,
        BSP,
        Conv,
        Events) {

    var getJobId = (function() {
        var jobId = 0;
        return function() {
            return jobId++;
        }
    })();

    // Create a worker and an event broker. The event broker
    // will beuser by the caller to listen to the job results
    var worker = new Worker('/ui/js/src/lathe/worker.js');

    var eventProxy = new function() {
        _.extend(this, Events);
    }
   
    worker.onerror = function () {
        console.error("worker error", arguments);
    }

    worker.onmessage = function(evt) {
        if (evt.data.hasOwnProperty('id')) {
            console.info('received job', evt.data.id);
            eventProxy.trigger(evt.data.id, evt.data.polygons);
        } else if (evt.data.error) {
            console.error('worker error:', evt.data.error);
        }
    }

    var createSphere = function(x,y,z,r) {
        var jobId = getJobId();
        worker.postMessage({id: jobId, sphere: [x,y,z,r,24]});
        return jobId;
    }

    var createCube = function(x,y,z,w,d,h) {
        var jobId = getJobId();
        worker.postMessage({id: jobId, cube: [x,y,z,w,d,h]});
        return jobId;
    }

    // var subtract = function(a,b) {
    //     return BSP.difference(a, b, Polygon3D);
    // }

    // var toBrep = function(bsp) {
    //     return Conv.bspToBrep(bsp);
    // }

    return {
        createCube  : createCube,
        createSphere: createSphere,
        broker      : eventProxy,
    }

});