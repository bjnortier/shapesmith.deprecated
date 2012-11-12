var chai = require('chai'),
    assert = chai.assert,
    client = require('./client').client;
chai.Assertion.includeStack = true;

describe('Undo/Redo', function() {

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

    it('can undo/redo point creation', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .point')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .click('.toolbar .select')
            .assertNumberOfDisplayNodes(2)
            .back()
            .assertNumberOfDisplayNodes(1)
            .back()
            .assertNumberOfDisplayNodes(0)
            .forward()
            .assertNumberOfDisplayNodes(1)
            .forward()            
            .assertNumberOfDisplayNodes(2, done)
    });

    it('can undo/redo point editing', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .point')
            .clickOnWorld(0,0,0)
            .click('.toolbar .select')
            .assertNumberOfDisplayNodes(1)
            .clickOnWorld(0,0,0)
            .dragToWorld(-10,-10,0)
            .assertNumberOfEditingNodes(1)
            .assertNumberOfDisplayNodes(0)
            .assertTextEqual('.vertex.editing.point0 .coordinate', '-10-100')
            // Finish edit
            .clickOnWorld(0,0,0) 
            .pause(500)
            // Should be back at 0,0,0
            .back()
            .clickOnWorld(0,0,0) 
            .assertNumberOfEditingNodes(1)
            .assertNumberOfDisplayNodes(0)
            .assertTextEqual('.vertex.editing.point0 .coordinate', '000')
            .clickOnWorld(50,50,50) 
            // Undo edit
            .pause(2000, done)
    });

    it("doesn't create a command when the SHA is unchanged", function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .point')
            .clickOnWorld(0,0,0)
            .click('.toolbar .select')
            .assertNumberOfDisplayNodes(1)
            .clickOnWorld(0,0,0)
            .assertNumberOfEditingNodes(1)
            // Finish edit
            .clickOnWorld(10,10,10) 
            .pause(500)
            .back() // Should undo point creation
            .assertNumberOfDisplayNodes(0)
            .pause(2000, done)
    });

    it("can undo/redo geometry with implicit children (a polyline)", function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .dblClickOnWorld(20,20,0)
            .click('.toolbar .select')
            .assertNumberOfDisplayNodes(1)
            .back() 
            .assertNumberOfDisplayNodes(0)
            .forward()
            .assertNumberOfDisplayNodes(1)
            .pause(2000, done)
    });

    it("can undo/redo editing a polyline by dragging points", function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .dblClickOnWorld(20,20,0)
            .click('.toolbar .select')
            .moveToWorld(0,0,0)
            .dragToWorld(-10,-10,0)
            .pause(500)
            .back()
            .clickOnWorld(0,0,0)
            .assertNumberOfEditingNodes(3)
            .assertTextEqual('.vertex.editing.point0 .coordinate', '000')
            .assertTextEqual('.vertex.editing.point1 .coordinate', '20200')
            .clickOnWorld(30,30,30)
            .forward()
            .clickOnWorld(-10,-10,0)
            .assertNumberOfEditingNodes(3)
            .assertTextEqual('.vertex.editing.point0 .coordinate', '-10-100')
            .assertTextEqual('.vertex.editing.point1 .coordinate', '20200', done)
    });

    it("can undo/redo editing a polyline as a whole", function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .dblClickOnWorld(20,20,0)
            .click('.toolbar .select')
            .clickOnWorld(0,0,0)
            .moveToWorld(0,0,0)
            .dragToWorld(-10,-10,0)
            .moveToWorld(20,20,0)
            .dragToWorld(30,30,0)
            .clickOnWorld(50,50,0)
            .pause(500)
            .back()
            .clickOnWorld(0,0,0)
            .assertNumberOfEditingNodes(3)
            .assertTextEqual('.vertex.editing.point0 .coordinate', '000')
            .assertTextEqual('.vertex.editing.point1 .coordinate', '20200')
            .clickOnWorld(30,30,30)
            .forward()
            .clickOnWorld(-10,-10,0)
            .assertNumberOfEditingNodes(3)
            .assertTextEqual('.vertex.editing.point0 .coordinate', '-10-100')
            .assertTextEqual('.vertex.editing.point1 .coordinate', '30300', done)
    });

});

