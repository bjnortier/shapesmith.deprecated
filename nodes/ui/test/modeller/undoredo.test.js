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
            .waitForUrlChange(
                function() { client.clickOnWorld(0,0,0) }, // point0
                function() {
                    client
                        .waitForUrlChange(
                            function() { client.clickOnWorld(10,10,0); }, // point1
                            function() {
                                client
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
                });
    }); 

    it("doesn't create a command when the SHA is unchanged", function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .point')
            .waitForUrlChange(
                function() { client.clickOnWorld(0,0,0); },
                function() {
                    client  
                        .click('.toolbar .select')
                        .assertNumberOfDisplayNodes(1)
                        .clickOnWorld(0,0,0)
                        .assertNumberOfEditingNodes(1)
                        .clickOnWorld(10,10,10)
                        .back() 
                        .assertNumberOfDisplayNodes(0, done)
                });
    });

    it("can undo/redo geometry with implicit children (a polyline)", function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .waitForUrlChange(
                function() { client.dblClickOnWorld(20,20,0) },
                function() {
                    client
                        .click('.toolbar .select')
                        .assertNumberOfDisplayNodes(1)
                        .back() 
                        .assertNumberOfDisplayNodes(0)
                        .forward()
                        .assertNumberOfDisplayNodes(1, done);
                });
    });

    it("can undo/redo editing a polyline as a whole", function(done) {
        this.timeout(10000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .waitForUrlChange(
                function() {
                    client.dblClickOnWorld(20,20,0)
                }, 
                function() {
                    client
                        .click('.toolbar .select')
                        .clickOnWorld(0,0,0)
                        .moveToWorld(0,0,0)
                        .dragToWorld(-10,-10,0)
                        .moveToWorld(20,20,0)
                        .dragToWorld(20,0,0)
                        .waitForUrlChange(
                            function() { client.clickOnWorld(-20,-20,0) },
                            function() {
                                client
                                    .back()
                                    .clickOnWorld(0,0,0)
                                    .assertNumberOfEditingNodes(3)
                                    .assertCoordinateEqual('.vertex.editing.point0 .coordinate', 0,0,0)
                                    .assertCoordinateEqual('.vertex.editing.point1 .coordinate', 20,20,0)
                                    .clickOnWorld(15,15,0)
                                    .forward()
                                    .clickOnWorld(-10,-10,0)
                                    .assertNumberOfEditingNodes(3)
                                    .assertCoordinateEqual('.vertex.editing.point0 .coordinate', -10,-10,0)
                                    .assertCoordinateEqual('.vertex.editing.point1 .coordinate', 20,0,0, done)
                            });

                });
    });

});

