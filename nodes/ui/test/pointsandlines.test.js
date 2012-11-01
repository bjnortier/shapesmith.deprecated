var chai = require('chai'),
    assert = chai.assert,
    webdriverjs = require("webdriverjs"),
    client = webdriverjs.remote({
        logLevel: 'silent',
        desiredCapabilities:{
            browserName:"chrome",
        }
    });
chai.Assertion.includeStack = true;

describe('Modelling User Interface', function() {

    before(function(done) {
        client.addCommand('assertTextEqual', function(selector, text, callback) {
            this.getText(selector, function(result) {
                assert.equal(text, result.value, selector);
                callback();
            });
        });
        client.addCommand('assertCoordinateEqual', function(selector, x, y, z, callback) {
            this.assertTextEqual(selector + ' .x', x);
            this.assertTextEqual(selector + ' .y', y);
            this.assertTextEqual(selector + ' .z', z);
            client.pause(0, callback);
        });
        client.addCommand('moveToWorld', function(x,y,z, callback) {
            this.execute('return SS.toScreenCoordinates('+x+','+y+','+z+')', function(result) {
                var xOffset = Math.round(result.value.x);
                var yOffset = Math.round(result.value.y);
                client.element('css selector', '#scene', function(result) {
                    var element = result.value.ELEMENT;
                    client
                        .moveTo(element, xOffset, yOffset, callback)
                });
            });
        });
        client.addCommand('clickOnWorld', function(x,y,z, callback) {
            this
                .moveToWorld(x,y,z)
                .buttonDown()
                .buttonUp(callback);
        });
        client.init(function() {
            done();
        });
    });

    beforeEach(function(done) {
        this.timeout(5000);
        client
            .url('http://localhost.shapesmith.net:8000/local/testdesign/modeller')
            .execute('SS.dontDampTrackball(); SS.zoomIn(); ', function() {
                done();
            })

    });

    it('can create a polyline with the mouse', function(done) {
        this.timeout(20000);
        client.waitFor('.toolbar', 2000, function() {
            client
                .click('.toolbar .polyline')
                .clickOnWorld(0,0,0)
                .clickOnWorld(10,0,0)
                .clickOnWorld(10,10,0)
                .clickOnWorld(0,10,0)
                .clickOnWorld(0,0,0, function() {
                    client.assertCoordinateEqual('.point._0', 0, 0, 0)
                    client.assertCoordinateEqual('.point._1', 10, 0, 0)
                    client.assertCoordinateEqual('.point._2', 10, 10, 0)
                    client.assertCoordinateEqual('.point._3', 0, 10, 0)
                    client.assertCoordinateEqual('.point._4', 0, 0, 0, done);
                });
        });
    }); 

    it('can create a polyline that uses a predefined point', function(done) {
        this.timeout(20000);
        client.waitFor('.toolbar', 2000, function() {
            client
                .click('.toolbar .point')
                .clickOnWorld(5,5,0)
                .click('.okcancel .ok')
                .click('.toolbar .polyline')
                .clickOnWorld(0,0,0)
                .clickOnWorld(5,5,0)
                .clickOnWorld(5,0,0)
                .clickOnWorld(0,0,0,function() {
                    client.assertCoordinateEqual('.point._0', 0, 0, 0)
                    client.assertCoordinateEqual('.point._2', 5, 0, 0)
                    client.assertCoordinateEqual('.point._3', 0, 0, 0)
                    client.getText('.point._1', function(result) {
                        done();
                    });
                });
        });
    }); 

    it('can select a point', function() {
        this.timeout(20000);
        client.waitFor('.toolbar', 2000, function() {
            client
                .click('.toolbar .point')
                .getText('.vertex.editing .title .name', function(result) {
                    var name = result.value;
                    client
                        .clickOnWorld(0,0,0)
                        .click('.okcancel .ok')
                        .click('.vertex.display.' + name, function(result) {
                            console.log(result);
                            done();
                        });
                });
        });
    });


    it('can select a point in the graph', function(done) {
        this.timeout(20000);
        client.waitFor('.toolbar', 2000, function() {
            client
                .click('.toolbar .point')
                .getText('.vertex.editing .title .name', function(result) {
                    var name = result.value;
                    var vertexGraphElementCSS = '.vertex.display.' + name;
                    client
                        .clickOnWorld(20,10,0)
                        .click('.okcancel .ok')
                        .click(vertexGraphElementCSS, function() {
                            client.getAttribute(vertexGraphElementCSS, 'class', function(result) {
                                assert.include(result, 'selected');
                                client
                                    .clickOnWorld(0,0,0)
                                    .getAttribute(vertexGraphElementCSS, 'class', function(result) {
                                        assert.equal(-1, result.indexOf('selected'));
                                        done();
                                    });

                            });
                        });

                });
        });
    }); 

    it('can select a point in the scene', function(done) {
        this.timeout(20000);
        client.waitFor('.toolbar', 2000, function() {
            client
                .click('.toolbar .point')
                .getText('.vertex.editing .title .name', function(result) {
                    var name = result.value;
                    var vertexGraphElementCSS = '.vertex.display.' + name;
                    client
                        .clickOnWorld(20,10,0)
                        .click('.okcancel .ok')
                        .clickOnWorld(20,10,0, function() {
                            client.getAttribute(vertexGraphElementCSS, 'class', function(result) {
                                assert.include(result, 'selected');
                                client
                                    .clickOnWorld(0,0,0)
                                    .getAttribute(vertexGraphElementCSS, 'class', function(result) {
                                        assert.equal(-1, result.indexOf('selected'));
                                        done();
                                    });

                            });
                        });

                });
        });
    }); 

    after(function(done) {
        client.end(done);
    });

});
