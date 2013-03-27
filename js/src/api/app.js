var express = require('express');
    fs = require('fs'),
    path = require('path'),
    nconf = require('nconf'),
    app = express();

var requirejs = require('requirejs');
requirejs.config({
    baseUrl: path.normalize(__dirname + '/../..'),
    nodeRequire: require,
});

// ---------- Configuration ----------
nconf.argv()
     .file({file: __dirname + '/config/devel.config.json' });
var diskDBPath = path.normalize(path.join(__dirname, nconf.get('diskDBPath')));
console.info('configuration:');
console.info('--------------');
console.info('diskDBPath:',diskDBPath);

// ---------- Create db ----------
var DB = requirejs('src/api/disk_db');
var db = new DB({root: diskDBPath});

app.set('view engine', 'hbs');
app.set('views', __dirname + '/templates');

app.use('/ui', express.static(__dirname + '/../../../'));

// app.use(express.logger());

app.use(express.bodyParser());
app.use(function(err, req, res, next){
  console.error(err.stack);
  res.send(500, 'Oops. An error occurred.');
});

app.get('/', function(req, res) {
  res.redirect('/_ui/design');
});

// Designs UI
app.get(/^\/_ui\/([\w%]+)\/designs$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  res.render('designs', {user: user});
});

// Designs API
app.get(/^\/_api\/([\w%]+)\/designs$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  db.getDesigns(user, function(err, data) {
    if (err) {
      res.send(500, err);
    } else {
      return res.json(data);
    }
  });
});

// Create design
// TODO: Name doesn't exist
// TODO: Name is valid
app.put(/^\/_api\/([\w%]+)\/([\w%]+)\/?$/, function(req, res) {

  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);

  // 1. Create the path for the designs
  // 2. Create the empty graph
  // 3. Create the refs
  // 4. Add the design to the list of designs

  db.createDesignPath(user, design, function(err) {
    if (err) {
      res.send(500, err);
    } else {

      var emptyGraph = {
        vertices: [],
        edges: [],
        metadata: [],
      }

      db.createGraph(user, design, emptyGraph, function(err, sha) {
        if (err) {
          res.send(500, err);
        } else {

          var refs = {
            'heads' : {
              'master': sha
            }
          }

          db.createRefs(user, design, refs, function(err) {
            if (err) {
              res.send(500, err)
            } else {

              db.addDesign(user, design, function(err) {
                if (err) {
                  res.send(500, err);
                } else {
                  res.json(refs);
                }
              });
            }
          });
        }
      });
    }
  });
});

// Rename design.
// NB! This is not safe if multiple requests change
// the list of designs at the same time!
app.post(/^\/_api\/([\w%]+)\/([\w%]+)\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  if (!req.body.newName) {
    res.json(400, 'no newName parameter');
  } else if (!/^[a-zA-Z_][a-zA-Z0-9-_\\s]*$/.test(req.body.newName)) {
    res.json(400, 'invalid new name');
  } else {
    var newName = req.body.newName;
    db.renameDesign(user, design, newName, function(err, data) {
      if (err) {
        if (err === 'alreadyExists') {
          res.send(409, 'already exists');
        } else {
          res.send(500, err);
        }
      } else {
        res.json(data);
      }
    });
  }
});

// Delete design
app.delete(/^\/_api\/([\w%]+)\/([\w%]+)\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  db.deleteDesign(user, design, function(err, data) {
    if (err) {
      if (err === 'notFound') {
        res.send(404, 'not found');
      } else {
        res.send(500, err);
      }
    } else {
      res.json(data);
    }
  });
});

// Get Refs
app.get(/^\/_api\/([\w%]+)\/([\w%]+)\/refs$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  db.getRefs(user, design, function(err, data) {
    if (err) {
      res.send(500, err)
    } else {
      res.json(data);
    }
  });
});

// Update ref
app.put(/^\/_api\/([\w%]+)\/([\w%]+)\/refs\/(\w+)\/(\w+)\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  var type = req.params[2];
  var ref = req.params[3];
  db.updateRefs(user, design, type, ref, req.body, function(err, data) {
    if (err) {
      res.send(500, err)
    } else {
      res.json(data);
    }
  });

});

// Modeller UI
app.get(/^\/_ui\/([\w%]+)\/([\w%]+)\/modeller$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  res.render('modeller', {user: user, design: design});
});


// Create graph
app.post(/^\/_api\/([\w%]+)\/([\w%]+)\/graph\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  var graph = req.body;
  db.createGraph(user, design, graph, function(err, sha) {
    if (err) {
      res.send(500, err);
    } else {
      res.json(sha);
    }
  });
});

// Get graph
app.get(/^\/_api\/([\w%]+)\/([\w%]+)\/graph\/([\w%]+)\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  var sha = req.params[2];
  db.getGraph(user, design, sha, function(err, data) {
    if (err) {
      res.send(500, err);
    } else {
      return res.json(data);
    }
  });
});

// Create vertex
app.post(/^\/_api\/([\w%]+)\/([\w%]+)\/vertex\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  var vertex = req.body;
  db.createVertex(user, design, vertex, function(err, sha) {
    if (err) {
      res.send(500, err);
    } else {
      res.json(sha);
    }
  });
});

// Get vertex
app.get(/^\/_api\/([\w%]+)\/([\w%]+)\/vertex\/([\w%]+)\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  var sha = req.params[2];
  db.getVertex(user, design, sha, function(err, data) {
    if (err) {
      res.send(500, err);
    } else {
      return res.json(data);
    }
  });
});

// For controlling the process (e.g. via Erlang) - stop the server
// when stdin is closed
process.stdin.resume();
process.stdin.on('end', function() {
  process.exit();
});

app.listen(8100);
console.info('--------------');
console.info('server started on :8100');
