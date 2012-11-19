var chai = require('chai'),
    assert = chai.assert,
    client = require('./client').client;
chai.Assertion.includeStack = true;

describe('Deletion', function() {

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

    it('can delete a point', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .point')
            .waitForUrlChange(
                function() { client.clickOnWorld(5,5,0); },
                function() {
                    client
                        .click('.toolbar .select')
                        .click('.point0 .delete')
                        .assertNumberOfDisplayNodes(0)
                        .back()
                        .assertNumberOfDisplayNodes(1)
                        .forward()
                        .assertNumberOfDisplayNodes(0, done)
                });
    });

    it.skip('can delete a selected point', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .point')
            .waitForUrlChange(
                function() { client.clickOnWorld(5,5,0); },
                function() {
                    client
                        .click('.toolbar .select')
                        .click('.point0')
                        .click('.point0 .delete')
                        .assertNumberOfDisplayNodes(0)
                        .back()
                        .assertNumberOfDisplayNodes(1)
                        .forward()
                        .assertNumberOfDisplayNodes(0, done);
                });


    });

    it('can delete a polyline', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(0,5,0)
            .clickOnWorld(5,5,0)
            .waitForUrlChange(
                function() { client.clickOnWorld(0,0,0); },
                function() {
                    client
                        .click('.toolbar .select')
                        .waitForUrlChange(
                            function() { client.click('.polyline0 .delete'); },
                            function() {
                                client
                                    .assertNumberOfDisplayNodes(0)
                                    .back()
                                    .assertNumberOfDisplayNodes(1)
                                    .forward()
                                    .assertNumberOfDisplayNodes(0, done);
                                });

                });
    });

    it('can delete a variable', function(done) {
        this.timeout(5000)
        client
            .click('#variables .add')
            .assertNumberOfEditingNodes(1)
            .setValue('#variables .name input', 'a')
            .setValue('#variables .expression input', '1')
            .waitForUrlChange(
                function() { client.clickOnWorld(0,0,0); },
                function() {
                    client
                        .assertNumberOfDisplayNodes(1)
                        .waitForUrlChange(
                            function() { client.click('#variables .a .delete'); },
                            function() {
                                client.assertNumberOfDisplayNodes(0, done)
                            });
                });

    });

    it('can delete a variable being edited', function(done) {
        this.timeout(5000)
        client
            .click('#variables .add')
            .assertNumberOfEditingNodes(1)
            .setValue('#variables .name input', 'a')
            .setValue('#variables .expression input', '1')
            .waitForUrlChange(
                function() { client.clickOnWorld(0,0,0); },
                function() {
                    client
                        .assertNumberOfDisplayNodes(1)
                        .click('#variables .a')
                        .waitForUrlChange(
                            function() { client.click('#variables .delete'); },
                            function() {
                                client.assertNumberOfDisplayNodes(0, done)
                            });
                });

    });

});