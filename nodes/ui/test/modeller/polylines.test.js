var chai = require('chai'),
    assert = chai.assert,
    client = require('./client').client;
chai.Assertion.includeStack = true;

describe('Polylines', function() {

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

    it('can be created on the workplane', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .waitForUrlChange(
                function() { client.dblClickOnWorld(0,10,0); },
                function() {
                    client
                        .click('.toolbar .select')
                        .assertNumberOfEditingNodes(0)
                        .assertNumberOfDisplayNodes(1, done);
                });
    });

    it('can be created by re-using a point', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .clickOnWorld(0,10,0)
            .clickOnWorld(20,0,0)
            .clickOnWorld(10,10,0)
            .assertNumberOfEditingNodes(2)
            .assertNumberOfDisplayNodes(4, done)
    });


    it('ends the polyline when the first coordinate is clicked', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .clickOnWorld(0,10,0)
            .waitForUrlChange(
                function() { client.clickOnWorld(0,0,0); },
                function() {
                    client
                        .click('.toolbar .select')
                        .assertNumberOfDisplayNodes(1)
                        .assertNumberOfEditingNodes(0, done);
                });
    });

    it('ends the polyline when the first coordinate is double-clicked', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .clickOnWorld(0,10,0)
            .waitForUrlChange(
                function() { client.dblClickOnWorld(0,0,0); },
                function() {
                    client
                        .click('.toolbar .select')
                        .assertNumberOfDisplayNodes(1)
                        .assertNumberOfEditingNodes(0, done);
                });
    })

    it('can finish on an existing point', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .point')
            .waitForUrlChange(
                function() { client.clickOnWorld(0,0,0); },
                function() {
                    client
                        .click('.toolbar .polyline')
                        .clickOnWorld(0,0,0)
                        .clickOnWorld(10,10,0)
                        .clickOnWorld(0,10,0)
                        .waitForUrlChange(
                            function() { client.dblClickOnWorld(10,10,0); },
                            function() {
                                client
                                    .click('.toolbar .select')
                                    .assertNumberOfDisplayNodes(2, done)
                            });
                });
    });

    it('can edited by dragging the points', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,20,0)
            .clickOnWorld(10,0,0)
            .clickOnWorld(20,20,0)
            .clickOnWorld(20,0,0)
            .clickOnWorld(30,20,0)
            .waitForUrlChange(
                function() { client.dblClickOnWorld(30,0,0); },
                function() {
                    client
                        .click('.toolbar .select')
                        .moveToWorld(0,0,0)
                        .buttonDown()
                        .moveToWorld(0,-20,0)
                        .moveToWorld(0,-20,0)
                        .assertCoordinateEqual('.vertex.editing .coordinate', 0, -20, 0, done)
                });
    });

    it('can be cancelled when an implicit point is shared', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .clickOnWorld(20,10,0)
            .clickOnWorld(10,10,0)
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
            .waitForUrlChange(
                function() { client.dblClickOnWorld(0,0,0); },
                function() {
                    client
                        .click('.toolbar .select')
                        .clickOnWorld(10,10,0)
                        .assertCoordinateEqual('.vertex.editing.point0 .coordinate', 0,0,0)
                        .assertCoordinateEqual('.vertex.editing.point1 .coordinate', 10,10,0)
                        .assertCoordinateEqual('.vertex.editing.point2 .coordinate', 0,10,0)
                        .moveToWorld(10,10,0)
                        .dragToWorld(20,20,0)
                        .assertCoordinateEqual('.vertex.editing.point1 .coordinate', 20,20,0)
                        .waitForUrlChange(
                            function() { client.clickOnWorld(30,30,0); },
                            function() {
                                client
                                    .assertNumberOfEditingNodes(0)
                                    .assertNumberOfDisplayNodes(1, done)
                            });
                });
    });

    it('can be edited by selecting another one', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .clickOnWorld(0,10,0)
            .waitForUrlChange(
                function() { client.clickOnWorld(0,0,0); },
                function() {
                    client
                        .clickOnWorld(50,0,0)
                        .clickOnWorld(50,50,0)
                        .clickOnWorld(0,50,0)
                        .waitForUrlChange(
                            function() { client.clickOnWorld(50,0,0); },
                            function() {
                                client
                                .click('.toolbar .select')
                                    .clickOnWorld(10,10,0)
                                    .assertNumberOfEditingNodes(4)
                                    .assertNumberOfDisplayNodes(1)
                                    .assertCoordinateEqual('.vertex.editing.point0 .coordinate', 0,0,0)
                                    .assertCoordinateEqual('.vertex.editing.point1 .coordinate', 10,10,0)
                                    .assertCoordinateEqual('.vertex.editing.point2 .coordinate', 0,10,0)
                                    .clickOnWorld(50,50,0)
                                    .assertNumberOfEditingNodes(4)
                                    .assertNumberOfDisplayNodes(1)
                                    .assertCoordinateEqual('.vertex.editing.point4 .coordinate', 50,0,0)
                                    .assertCoordinateEqual('.vertex.editing.point5 .coordinate', 50,50,0)
                                    .assertCoordinateEqual('.vertex.editing.point6 .coordinate', 0,50,0)
                                    .clickOnWorld(30,30,0)
                                    .assertNumberOfEditingNodes(0)
                                    .assertNumberOfDisplayNodes(2, done)
                            });
                });
    });

    it("doesn't create extra points on double-click ending", function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .waitForUrlChange(
                function() { client.dblClickOnWorld(20,20,0); },
                function() {
                    client
                        .click('.toolbar .select')
                        .click('.polyline0')
                        .assertNumberOfEditingNodes(4)
                        .assertNumberOfDisplayNodes(0, done);
                });
    });


});

