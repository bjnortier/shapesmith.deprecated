var chai = require('chai'),
    assert = chai.assert,
    webdriverjs = require("webdriverjs"),
    client = webdriverjs.remote({
        // logLevel: 'silent',
        desiredCapabilities:{
            browserName:"chrome",
        }
    });
chai.Assertion.includeStack = true;

describe('Modelling User Interface', function() {

    before(function(done) {
        client.addCommand('assertTextEqual', function(selector, text, callback) {
            this.getText(selector, function(result) {
                assert.equal(text, result.value);
                callback();
            });
        });
        client.addCommand('moveToWorld', function(x,y,z, callback) {
            this.execute('return SS.toScreenCoordinates('+x+','+y+','+z+')', function(result) {
                console.log(result);
                var xOffset = Math.round(result.value.x);
                var yOffset = Math.round(result.value.y);
                client.element('css selector', '#scene', function(result) {
                    console.log(result);
                    var element = result.value.ELEMENT;
                    client
                        .moveTo(element, xOffset, yOffset)
                        .pause(500, callback)
                });
            });
        });
        done();
    });

    beforeEach(function(done) {
        this.timeout(5000);
        client
            .init()
            .url('http://localhost.shapesmith.net:8000/local/testdesign/modeller')
            .execute('SS.dontDampTrackball(); SS.zoomIn(); ', function() {
                done();
            })

    });

    it('can create a point with the mouse', function(done) {
        this.timeout(20000);
        client.waitFor('.toolbar', 2000, function() {
            client
                .click('.toolbar .point')
                .moveToWorld(20,10,0, function() {
                    client
                        .assertTextEqual('.vertex.editing .coordinate .x', '20')
                        .assertTextEqual('.vertex.editing .coordinate .y', '10')
                        .assertTextEqual('.vertex.editing .coordinate .z', '0', done);
                });
        });
    }); 

    afterEach(function(done) {
        client.end(done);
    });

});
