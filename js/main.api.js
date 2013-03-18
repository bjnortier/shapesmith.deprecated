var requirejs = require('requirejs');

requirejs.config({
    baseUrl: '.',
    nodeRequire: require,
});

var api = requirejs('src/graphapi');

process.stdin.resume();
process.stdin.setEncoding('utf8');

process.stdin.on('data', function(string) {
    try {
        var msg = JSON.parse(string);
        if (msg[0] && (msg[0] === 'to_obj')) {
            process.stdout.write(JSON.stringify(api.toOBJ(msg[1], msg[2])));
        } else if (msg[0] && (msg[0] === 'hash_object')) {
            process.stdout.write(JSON.stringify(api.hashObject(msg[1])));
        } else {
            process.stdout.write(JSON.stringify({error: 'unknown call'}));
        }
    } catch (e) {
        process.stdout.write(JSON.stringify({error: e.message}));
    }
});

process.on('uncaughtException', function(e) {
    process.stdout.write(JSON.stringify({error: e.message}));
});

process.stdin.on('error', function (e) {
    process.stdout.write(JSON.stringify({error: e.message}));
});

process.stdin.on('end', function () {
    process.stdout.write(JSON.stringify({error: 'end'}));
});
