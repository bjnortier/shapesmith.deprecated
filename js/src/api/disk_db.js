define([
    'fs', 
    'path', 
    'util',
    '../graphapi',
  ], function(
    fs, 
    path, 
    util,
    graphAPI) {

  var DiskDB = function(config) {

    if (!config.root) {
      throw Error('no root directory for disk db');
    }
    var root = path.normalize(config.root);
    // Use sync function here as this is on app startup and we want to fail
    // immediately
    if (!fs.existsSync(root)) {
      throw Error(util.format('directory "%s" doesn\'t exist', root));
    }
    if (!fs.statSync(root).isDirectory()) {
      throw Error(util.format('directory "%s" is not a directory', root));   
    }

    console.log('disk db root:', root);

    this.createDesignPath = function(user, design, callback) {
      var designPath = path.join(root, user, design);
      fs.exists(designPath, function(exists) {
        if (exists) {
          callback(util.format('design path: "%s" already exists', designPath));
        } else {
          // Create the design directory with the graph and vertex buckets
          fs.mkdir(designPath, function(err) {
            if (err) {
              callback(err);
            } else {

              fs.mkdir(path.join(designPath, 'graph'), function(err) {
                if (err) {
                  callback(err);
                } else {

                  fs.mkdir(path.join(designPath, 'vertex'), function(err) {
                    if (err) {
                      callback(err);
                    } else {
                      callback();
                    }
                  });
                }
              });
            }
          });
        }
      });
    }

    this.createGraph = function(user, design, graph, callback) {
      var sha = graphAPI.hashObject(graph);
      var graphPath = path.join(root, user, design, 'graph', sha);
      fs.writeFile(graphPath, JSON.stringify(graph), function (err) {
        if (err) {
          callback(err);
        } else {
          callback(undefined, sha);
        }
      });
    }

    this.getGraph = function(user, design, sha, callback) {
      var graphPath = path.join(root, user, design, 'graph', sha);
      fs.readFile(graphPath, function (err, data) {
        if (err) {
          callback(err);
        } else {
          callback(undefined, JSON.parse(data));
        }
      });
    }


    this.createVertex = function(user, design, vertex, callback) {
      var sha = graphAPI.hashObject(vertex);
      var graphPath = path.join(root, user, design, 'vertex', sha);
      fs.writeFile(graphPath, JSON.stringify(vertex), function (err) {
        if (err) {
          callback(err);
        } else {
          callback(undefined, sha);
        }
      });
    }

    this.getVertex = function(user, design, sha, callback) {
      var vertexPath = path.join(root, user, design, 'vertex', sha);
      fs.readFile(vertexPath, function (err, data) {
        if (err) {
          callback(err);
        } else {
          callback(undefined, JSON.parse(data));
        }
      });
    }

    this.addDesign = function(user, design, callback) {

      var designsPath = path.join(root, user, '_designs');
      fs.readFile(designsPath, function (err, data) {
        if (err) {
          if (err.code === 'ENOENT') {
            // Create hte designs file
            fs.writeFile(designsPath, JSON.stringify([design]), function(err) {
              callback(err);
            });
          } else {
            callback(err);
          }
        } else {
          // Add the new design
          var designs = JSON.parse(data).concat([design]);
          fs.writeFile(designsPath, JSON.stringify(designs), function(err) {
            callback(err);
          });
        }
      });

    }

    this.getDesigns = function(user, callback) {

      var designsPath = path.join(root, user, '_designs');
      fs.readFile(designsPath, function (err, data) {
        if (err) {
          if (err.code === 'ENOENT') {
            callback(undefined, []);
          } else {
            callback(err);
          }
        } else {
          callback(undefined, JSON.parse(data));
        }
      });
    }

    this.createRefs = function(user, design, refs, callback) {
      var designPath = path.join(root, user, design);
      var refsPath = path.join(designPath, '_refs');
      fs.writeFile(refsPath, JSON.stringify(refs), function (err) {
        if (err) {
          callback(err);
        } else {
          callback();
        }
      });
    }

  }

  return DiskDB;

});