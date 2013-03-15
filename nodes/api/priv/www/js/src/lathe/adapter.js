// The adapter is the interface between vertices and lathe objects
define([
        'src/casgraph/sha1hasher',
        'src/lathe/pool',
        'src/lathe/bspdb',
    ], function(SHA1Hasher, Lathe, bspdb) {

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
                    bspdb.write(jobResult, function(err) {
                        if (err) {
                            console.error('error writing to BSP DB', err);
                            callback(err);
                        } else {
                            callback(undefined, jobResult);
                        }
                    })
                })
            }
        });
    }

    var generateSphere = function(x,y,z,r,callback) {
        var sha = SHA1Hasher.hash({x: x, y:y, z:z, r:r});
        getOrGenerate(sha, function() {
            return Lathe.createSphere(x,y,z,r);
        }, callback);
    }

    var generateCube = function(x,y,z,w,d,h,callback) {
        var sha = SHA1Hasher.hash({x: x, y:y, z:z, w:w, d:d, h:h});
        getOrGenerate(sha, function() {
            return Lathe.createCube(x,y,z,w,d,h);
        }, callback);
    }

    return {
        generateCube   : generateCube,
        generateSphere : generateSphere,
    }

});