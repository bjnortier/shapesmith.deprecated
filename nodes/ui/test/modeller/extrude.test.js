var chai = require('chai'),
    assert = chai.assert,
    client = require('./client').client;
chai.Assertion.includeStack = true;

describe('Extrusions', function() {

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
            .waitForUrlChange(
                function() { client.dblClickOnWorld(0,10,0); },
                function() {
                    client
                        .click('.toolbar .select')
                        .clickOnWorld(0,0,0)
                        .click('.toolbar .extrude')
                        .assertNumberOfDisplayNodes(1)
                        .assertNumberOfEditingNodes(1)
                        .moveToWorld(0,0,1)
                        .dragToWorld(0,0,10)
                        .assertValueEqual('.vertex.editing.extrude0 .h', '10')
                        .waitForUrlChange(
                            function() { client.clickOnWorld(0,-10,0); },
                            function() {
                                client
                                    .assertNumberOfDisplayNodes(2)
                                    .assertNumberOfEditingNodes(0, done);
                            });
                });

    });

    it('can be edited by dragging the polyline points or the height anchors', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .waitForUrlChange(
                function() { client.dblClickOnWorld(0,10,0); },
                function() {
                    client
                        .click('.toolbar .select')
                        .clickOnWorld(0,0,0)
                        .click('.toolbar .extrude')
                        .moveToWorld(0,0,1)
                        .dragToWorld(0,0,10)
                        .waitForUrlChange(
                            function() { client.clickOnWorld(0,-10,0); },
                            function() {
                                // Drag the polyline corner
                                client
                                    .moveToWorld(0,0,0)
                                    .waitForUrlChange(
                                        function() { client.dragToWorld(0,-10,0); },
                                        function() {
                                            // Now the extrution should be at 0,-10,10
                                            client
                                                .clickOnWorld(0,-9,9)
                                                .assertNumberOfEditingNodes(1)
                                                .moveToWorld(0,-10,10)
                                                .dragToWorld(0,-10,20)
                                                .assertValueEqual(
                                                    '.vertex.editing.extrude0 .h', '20', done)
                                        });
                            });
                });
    });

});