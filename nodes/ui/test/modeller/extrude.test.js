var chai = require('chai'),
    assert = chai.assert,
    client = require('./client').client;
chai.Assertion.includeStack = true;

describe('Extrutions', function() {

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

    it('can be created from a polyline', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .dblClickOnWorld(10,10,0)
            .waitForUrlChange()
            .click('.toolbar .select')
            .clickOnWorld(0,0,0)
            .click('.toolbar .extrude')
            .assertNumberOfDisplayNodes(1)
            .assertNumberOfEditingNodes(1)
            .moveToWorld(0,0,1)
            .dragToWorld(0,0,10)
            .assertTextEqual('.vertex.editing.extrude0 .z', '10')
            .clickOnWorld(10,0,0)
            .waitForUrlChange()
            .assertNumberOfDisplayNodes(2)
            .assertNumberOfEditingNodes(0, done)

    });

    it('can be edited by dragging the polyline points or the height anchors', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .dblClickOnWorld(10,10,0)
            .waitForUrlChange()
            .click('.toolbar .select')
            .clickOnWorld(0,0,0)
            .click('.toolbar .extrude')
            .moveToWorld(0,0,1)
            .dragToWorld(0,0,10)
            .clickOnWorld(10,0,0)
            .waitForUrlChange() 
            .moveToWorld(0,0,0)
            .dragToWorld(-10,-10,0)
            .waitForUrlChange()
            // Now the extrution should be at -10,-10,10
            .clickOnWorld(-9,-9,9)
            .assertNumberOfEditingNodes(1)
            .moveToWorld(-10,-10,10)
            .dragToWorld(-10,-10,20)
            .assertTextEqual('.vertex.editing.extrude0 .z', '20', done)
    });

});