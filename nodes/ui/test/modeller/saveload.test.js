var chai = require('chai'),
    assert = chai.assert,
    client = require('./client').client;
chai.Assertion.includeStack = true;

describe('Save/Load', function() {

    before(function(done) {
        this.timeout(5000);
        client.initDesign(done);
            
    });

    beforeEach(function(done) {
        this.timeout(5000);
        client.freshDesign(done);

    });

    after(function(done) {
        client.end(done);
    });

    // ---------- Cases ----------

    it('can load a point', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .point')
            .clickOnWorld(0,0,0)
            .waitForUrlChange()
            .loadCommit()
            .assertNumberOfDisplayNodes(1, done);

    });

    it('can load a polyline', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .dblClickOnWorld(20,20,0)
            .waitForUrlChange()
            .loadCommit()
            .assertNumberOfDisplayNodes(1, done);

    });


    it("can load, edit and refresh a polyline", function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .dblClickOnWorld(20,20,0)
            .waitForUrlChange()
            .loadCommit()   
            .assertNumberOfDisplayNodes(1)
            .moveToWorld(0,0,0)
            .dragToWorld(-10,-10,0)
            .waitForUrlChange()
            .loadCommit()
            .assertNumberOfDisplayNodes(1)
            .clickOnWorld(-10,-10,0)
            .assertNumberOfEditingNodes(3)
            .assertTextEqual('.vertex.editing.point0 .coordinate', '-10-100')
            .assertTextEqual('.vertex.editing.point1 .coordinate', '20200', done)

    });


});