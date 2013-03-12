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

    // Create a worker pool and an event broker. The event broker
    // will be used by the caller to listen to the job results
    var poolSize = 4;
    var workers = [];
    for (var i = 0; i < poolSize; ++i) {
        workers[i] = new Worker('/ui/js/src/lathe/worker.js');
        workers[i].onerror = function () {
            console.error("worker error", arguments);
        }
    }

    // The job queue will manage the jobs, and put jobs in a queue
    // if there are no workers available. When a worker becomes available,
    // it will be assigned the next jon in the queue
    var JobQueue = function(workers) {

        // Event broker use to listen for events. Events not on the
        // queue itself so the job queue object is not exposed externally
        var broker = new function() {
            _.extend(this, Events);
        }
        this.broker = broker;

        var queue = [];
        var nextJobId = 0;

        var workers = workers;
        for (var i = 0; i < workers.length; ++i) {
            var worker = workers[i];
            worker.onmessage = function(evt) {
                // A worker returns either the result or an error,
                // and is then available
                if (evt.data.hasOwnProperty('id')) {
                    var bsp = BSP.deserialize(evt.data.bsp);
                    broker.trigger(evt.data.id, {
                        bsp: bsp, 
                        polygons: evt.data.polygons
                    });
                } else if (evt.data.error) {
                    console.error('worker error:', evt.data.error);
                } else {
                    console.error('unknown worker message', evt.data);
                }
                worker.busy = false;
                doNextJob(worker);
            }
        }

        this.queueJob = function(job) {
            var jobId = nextJobId++;
            job.id = jobId;
            var worker = getAvailableWorker();
            if (worker) {
                doJob(job, worker);
            } else {
                queue.push(job);
            }
            return jobId;
        }

        var doJob = function(job, worker) {
            worker.busy = true;
            worker.postMessage(job);
        }

        var doNextJob = function(worker) {
            if (queue.length) {
                var job = queue.shift();
                console.log('doing queued job', job.id);
                doJob(job, worker);
            }
        }

        var getAvailableWorker = function() {
            for (var i = 0; i < workers.length; ++i) {
                if (!workers[i].busy) {
                    return workers[i];
                }
            }
            return undefined;
        }
    }

    var jobQueue = new JobQueue(workers);

    var createSphere = function(x,y,z,r) {
        return jobQueue.queueJob({sphere: [x,y,z,r,16]})
    }

    var createCube = function(x,y,z,w,d,h) {
        return jobQueue.queueJob({cube: [x,y,z,w,d,h]});
    }

    var createSubtract = function(a,b) {
        return jobQueue.queueJob({subtract: [BSP.serialize(a), BSP.serialize(b)]});
    }

    // var toBrep = function(bsp) {
    //     return Conv.bspToBrep(bsp);
    // }

    return {
        createCube     : createCube,
        createSphere   : createSphere,
        createSubtract : createSubtract,
        broker         : jobQueue.broker,
    }

});