// The adapter is the interface between vertices and lathe objects
define([
        'casgraph/sha1hasher',
        'geomnode',
        'geometrygraphsingleton',
        'latheapi/normalize',
        'latheapi/pool',
        'latheapi/bspdb',
    ], function(SHA1Hasher, GeomNode, geometryGraph, Normalize, Lathe, BSPDB) {

    var infoHandler = function(a,b,c,d) {
        console.info.apply(console, arguments);
    }
    var errorHandler = function(a,b,c,d) {
        console.error.apply(console, arguments);
    }
    var bspdb = new BSPDB(infoHandler, errorHandler); 

    var getOrGenerate = function(sha, generator, callback) {
        // Read from the DB, or generate it if it doesn't exist
        bspdb.read(sha, function(err, jobResult) {
            if (err) {
                console.error('error reading from BSP DB', err);
            }
            if (jobResult) {
                callback(undefined, jobResult);
            } else {
                var jobId = generator();
                Lathe.broker.on(jobId, function(jobResult) {
                    jobResult.sha = sha;
                    jobResult.id = jobId;
                    callback(undefined, jobResult);
                });
            }
        });
    }

    var generateParent = function(vertex, callback) {

        var children = geometryGraph.childrenOf(vertex);
        var childSHAs = {};
        var allChildrenExist;

        // Ensure all children exist 
        var remaining = children.length;
        children.forEach(function(child) {
            generate(child, function(err, result) {
                if (err) {
                    callback(err)
                } else {
                    --remaining;
                    childSHAs[child.id] = result.sha;
                    if (remaining === 0) {
                        allChildrenExist();
                    }
                }
            })
        })

        allChildrenExist = function() {
            var uniqueObj = GeomNode.strip(vertex);
            uniqueObj.childSHAs = children.map(function(child) {
                return childSHAs[child.id];
            })
            var sha = SHA1Hasher.hash(uniqueObj);
            getOrGenerate(sha, function() {
                return Lathe.createSubtract(sha, uniqueObj.childSHAs);
            }, callback);
        }
        
    }

    var generate = function(vertex, callback) {
        switch (vertex.type) {
            case 'sphere':
                var normalized = Normalize.normalizeVertex(vertex);
                var sha = SHA1Hasher.hash(normalized);
                getOrGenerate(sha, function() {
                    return Lathe.createSphere(sha, normalized);
                }, callback);
                break;

            case 'cube':
                var normalized = Normalize.normalizeVertex(vertex);
                var sha = SHA1Hasher.hash(normalized);
                getOrGenerate(sha, function() {
                    return Lathe.createCube(sha, normalized);
                }, callback);
                break;
            case 'subtract':
                generateParent(vertex, callback);
                break;
            default:
                throw Error('unknown vertex id/type: ' + vertex.id + '/' + vertex.type);
        }
    }

    var broker = new function() {
            _.extend(this, Events);
        }
        this.broker = broker;

    var uiDBInitialized = false, poolInitialized = false;
    bspdb.on('initialized', function() {
        uiDBInitialized = true;
        if (uiDBInitialized && poolInitialized) {
            broker.trigger('initialized');
        }
    });
    Lathe.broker.on('initialized', function() {
        poolInitialized = true;
        if (uiDBInitialized && poolInitialized) {
            broker.trigger('initialized');
        }
    });

    return {
        generate: generate,
        broker  :  broker,
    }

});