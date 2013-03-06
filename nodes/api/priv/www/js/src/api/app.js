var express = require('express');
    fs = require('fs'),
    path = require('path'),
    nconf = require('nconf'),
    app = express(),

nconf.argv()
     .env()
     .file({ file: 'config/devel.config.json' });

var diskDBPath = nconf.get('diskDBPath');
console.info('configuration:');
console.info('--------------');
console.info('diskDBPath:' + diskDBPath);

app.set('view engine', 'hbs');
app.set('views', __dirname + '/templates');

app.use(express.static(__dirname + '/../../../public'));
// app.use(express.logger());
app.use(express.bodyParser());
app.use(function(err, req, res, next){
  console.error(err.stack);
  res.send(500, 'Oops. An error occurred.');
});

app.get('/hello.txt', function(req, res){
  res.send('Hello World');
});

// Designs UI
app.get(/^\/([\w%]+)\/designs.html$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  res.render('designs', {user: user});
});

// Designs API
app.get(/^\/([\w%]+)\/designs.json$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var filePath = path.join(diskDBPath, user, '_designs');
  fs.readFile(filePath, function (err, data) {
    if (err) {
      res.send(500, err)
    } else {
      res.send(data);
    }
  });
});

// Rename design.
// NB! This is not safe if multiple requests change
// the list of designs at the same time!
app.post(/^\/([\w%]+)\/([\w%]+)$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  if (!req.body.newName) {
    res.json(400, 'no newName parameter');
  } else if (!/^[a-zA-Z_][a-zA-Z0-9-_\\s]*$/.test(req.body.newName)) {
    res.json(400, 'invalid new name');
  } else {
    var designsPath = path.join(diskDBPath, user, '_designs');
    fs.readFile(designsPath, function (err, data) {
      if (err) {
        res.send(500, err);
      } else {
        var designs = JSON.parse(data);
        var newName = req.body.newName;
        var oldDirectoryPath = path.join(diskDBPath, user, design);
        var newdirectoryPath = path.join(diskDBPath, user, newName);

        var safeRename = function() {
          fs.rename(oldDirectoryPath, newdirectoryPath, function(err) {
            if (err) {
              res.send(500, err);
            } else {
              designs.splice(designs.indexOf(design), 1, newName);
              fs.writeFile(designsPath, JSON.stringify(designs), function(err) {
                if (err) {
                  res.send(500, err);
                } else {
                  res.json(200, 'ok');
                }
              });
            }
          });
        }

        fs.exists(newdirectoryPath, function(exists) {
          if (exists) {
            if (designs.indexOf(newName) === -1) {
              // Deleted design still lurking - delete the directory
              var deletedName = newdirectoryPath + '.deleted.' + new Date().getTime();
              fs.rename(newdirectoryPath, deletedName, function(err) {
                if (err) {
                  res.send(500, err);
                } else {
                  safeRename();
                }
              });
            } else {
              res.json(400, 'name already exists')
            }
          } else {
            safeRename();
          }
        });
      }
    })
  }
});

// Get Refs
app.get(/^\/([\w%]+)\/([\w%]+)\/refs$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);

  var filePath = path.join(diskDBPath, user, design, '_root');
  var refs = fs.readFile(filePath, function (err, data) {
    if (err) {
      res.send(500, err)
    } else {
      res.send(data);
    }
  });
});

// Update ref
app.put(/^\/([\w%]+)\/([\w%]+)\/refs\/(\w+)\/(\w+)$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  var type = req.params[2];
  var ref = req.params[3];

  var filePath = path.join(diskDBPath, user, design, '_root');
  var refs = fs.readFile(filePath, function (err, data) {
    if (err) {
      res.send(500, err)
    } else {
      var refsJson = JSON.parse(data);
      refsJson.refs[type][ref] = req.body;
      console.log(refsJson);
      fs.writeFile(filePath, JSON.stringify(refsJson), function (err) {
        if (err) {
          res.send(500, err);
        } else {
          res.type('json');
          res.json('ok');
        }
      });
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
console.info('Server started on :8100');
