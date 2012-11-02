var chai = require('chai'),
    assert = chai.assert,
    client = require('./client').client;
chai.Assertion.includeStack = true;

describe('Polylines', function() {

    before(function(done) {
        this.timeout(5000);
        client.init(function() {
            done();
        });
    });

    beforeEach(function(done) {
        this.timeout(2000);
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

    it('can be created on the workplane', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .dblClickOnWorld(0,10,0)
            .click('.toolbar .select')
            .elements('css selector', '.vertex.editing', function(result) {
                assert.equal(result.value.length, 0)
                client
                    .assertTextEqual('.vertex.display', 'polyline0')
                    .elements('css selector', '.vertex.display', function(result) {
                        assert.equal(result.value.length, 1);
                        done();
                    });

            });
    });

    it('can be created by re-using a point', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .clickOnWorld(0,10,0)
            .clickOnWorld(0,0,0)
            .assertTextEqual('.vertex.editing.polyline0 .point._0', 'point0')
            .assertTextEqual('.vertex.editing.polyline0 .point._1', 'point1')
            .assertTextEqual('.vertex.editing.polyline0 .point._2', 'point2')
            .assertTextEqual('.vertex.editing.polyline0 .point._3', 'point0')
            .assertTextEqual('.vertex.editing.polyline0 .point._4', 'point4', done)
    });


    it('can finish on an existing point', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .point')
            .clickOnWorld(0,0,0)
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .clickOnWorld(0,10,0)
            .dblClickOnWorld(0,0,0)
            .click('.toolbar .select')
            .elements('css selector', '.vertex.display', function(result) {
                assert.equal(result.value.length, 2);
                done();
            });
    });

    it('can edited by dragging the points', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,50,0)
            .clickOnWorld(10,0,0)
            .clickOnWorld(20,50,0)
            .clickOnWorld(20,0,0)
            .clickOnWorld(30,50,0)
            .dblClickOnWorld(30,0,0)
            .click('.toolbar .select')
            .moveToWorld(0,0,0)
            .buttonDown()
            .moveToWorld(0,-50,0)
            .moveToWorld(0,-50,0)
            .assertCoordinateEqual('.vertex.editing .coordinate', 0, -50, 0)
            .buttonUp(done)
    });

    it('can be cancelled when an implicit point is shared', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .clickOnWorld(0,10,0)
            .clickOnWorld(0,0,0)
            .click('.toolbar .select')
            .assertNumberOfEditingNodes(0)
            .assertNumberOfDisplayNodes(0, done);
    });

    it('can be edited', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .clickOnWorld(0,10,0)
            .dblClickOnWorld(0,0,0)
            .click('.toolbar .select')
            .clickOnWorld(10,10,0)
            .assertTextEqual('.vertex.editing.polyline0 .point._0', 'point0')
            .assertTextEqual('.vertex.editing.polyline0 .point._1', 'point1')
            .assertTextEqual('.vertex.editing.polyline0 .point._2', 'point2')
            .assertTextEqual('.vertex.editing.polyline0 .point._0', 'point0')
            .assertTextEqual('.vertex.editing.point0 .coordinate', '000')
            .assertTextEqual('.vertex.editing.point1 .coordinate', '10100')
            .assertTextEqual('.vertex.editing.point2 .coordinate', '0100')
            .moveToWorld(10,10,0)
            .dragToWorld(20,20,0)
            .assertTextEqual('.vertex.editing.point1 .coordinate', '20200')
            .clickOnWorld(30,30,0)
            .assertNumberOfEditingNodes(0)
            .assertNumberOfDisplayNodes(1, done)
    });


});

