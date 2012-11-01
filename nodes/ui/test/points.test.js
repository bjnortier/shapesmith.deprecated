var chai = require('chai'),
    assert = chai.assert,
    client = require('./client').client;
chai.Assertion.includeStack = true;

describe('Points', function() {

    before(function(done) {
        client.init(function() {
            done();
        });
    });

    beforeEach(function(done) {
        client
            .url('http://localhost.shapesmith.net:8000/local/testdesign/modeller')
            .execute('SS.dontDampTrackball(); SS.zoomIn(); ', function() {
                done();
            })

    });

    after(function(done) {
        client.end(done);
    });

    // ---------- Cases ----------

    it('can be created with subsequent mouse clicks', function(done) {
        this.timeout(10000);
        client
            .click('.toolbar .point')
            .getEditingVertexName(function(name) {
                client
                    .moveToWorld(20,10,0)
                    .assertCoordinateEqual('.vertex.editing .coordinate', 20, 10, 0)
                    .clickOnWorld(20,10,0)
                    .isVisible('.vertex.display.'+name, function(result) {
                        assert.isTrue(result);
                        client
                            .clickOnWorld(0,0,0)
                            .clickOnWorld(0,10,0)
                            .clickOnWorld(0,20,0)
                            .click('.toolbar .select')
                            .elements('css selector', '.vertex.display', function(result) {
                                assert.equal(result.value.length, 4);
                                done();
                            });

                    });
            });
    });

    it('can be edited with dragging', function(done) {
        this.timeout(10000);
        client
            .click('.toolbar .point')
            .clickOnWorld(5,5,0)
            .click('.toolbar .select')
            .moveToWorld(5,5,0)
            .buttonDown()
            .moveToWorld(15,15,0)
            .moveToWorld(15,15,0)
            .assertCoordinateEqual('.vertex.editing .coordinate', 15, 15, 0)
            .buttonUp(done);
    });


});
