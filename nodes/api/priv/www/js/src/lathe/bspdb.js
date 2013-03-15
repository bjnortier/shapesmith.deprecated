define([
        'underscore',
        'backbone-events',
        'lathe/bsp',
    ],
    function(_, Events, BSP) {

    var DB = function() {

        _.extend(this, Events);
        var that = this;
        var db;

        var openDBRequest = indexedDB.open("test15", 1);
        
        openDBRequest.onerror = function(event) {
            console.error('Could not create BSP database');
        }

        // Request success. Trigger initialized event if first success.
        var successCount = 0;
        openDBRequest.onsuccess = function(event) {
            if (!successCount) {
                that.trigger('initialized');
                console.info('BSP DB initialized');
                db = event.target.result;


                
            } 
            ++successCount;
        }

        // Will be called when the DB is created the first time or when
        // the version is older than the existing version
        openDBRequest.onupgradeneeded = function(event) {
            console.log('creating/upgrading BSP db');
            var db = event.target.result;
            bspStore = db.createObjectStore("bsp", { keyPath: "sha" });
        }

        this.read = function(sha, callback) {
            var transaction = db.transaction(["bsp"], "readonly");

            transaction.onerror = function(event) {
                console.error('could not read bsp', event);
                callback(event);
            }

            var request = transaction.objectStore("bsp").get(sha);
            request.onsuccess = function(event) {
                console.log('read success:', request.result && request.result.sha);
                callback(undefined, request.result);
            }

        }

        this.write = function(value, callback) {
            var transaction = db.transaction(["bsp"], "readwrite");
            transaction.onerror = function(event) {
                console.error('could not write bsp', event);
                callback(event);
            };
            transaction.oncomplete = function() {
                // console.info('write transaction complete');
            }

            var readRequest = transaction.objectStore("bsp").get(value.sha);
            readRequest.onsuccess = function(event) {
                if (readRequest.result) {
                    callback(undefined);
                } else {


                    // BSP is serialized manually otherwise the IndexDB shim fails
                    // because JSON.stringify fails because of circular references 
                    var writeRequest = transaction.objectStore("bsp").add({
                        sha: value.sha, 
                        bsp: BSP.serialize(value.bsp),
                        polygons: value.polygons
                    });
                    writeRequest.onsuccess = function(event) {
                        // console.log('write success', value.sha);
                        callback(undefined);
                    }
                    writeRequest.onerror = function(event) {
                        console.error('write request error', event);
                    }
                }
            }

            readRequest.onerror = function(event) {
                console.error('read error during write', event);
            }
        }

       
 


    }

    return new DB;

});