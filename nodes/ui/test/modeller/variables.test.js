var chai = require('chai'),
    assert = chai.assert,
    client = require('./client').client;
chai.Assertion.includeStack = true;

describe('Variables', function() {

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

    it("can't be create two-at-once", function(done) {
        this.timeout(5000)
        client
            .click('#variables .add')
            .assertNumberOfEditingNodes(1)
            .click('#variables .add')
            .assertNumberOfEditingNodes(1, done);

    });


    it.only("can't be edit more than one at a time", function(done) {
        this.timeout(5000)
        client
            .click('#variables .add')
            .setValue('#variables .name input', 'a')
            .setValue('#variables .expression input', '1')
            .waitForUrlChange(
                function() { client.clickOnWorld(0,0,0); },
                function() {
                    client
                        .click('#variables .add')
                        .setValue('#variables .name input', 'b')
                        .setValue('#variables .expression input', '2')
                        .waitForUrlChange(
                            function() { client.clickOnWorld(0,0,0); },
                            function() {
                                client
                                    .assertNumberOfDisplayNodes(2)
                                    .click('#variables .a')
                                    .assertNumberOfDisplayNodes(1)
                                    .assertNumberOfEditingNodes(1)
                                    .pause(1000)
                                    .click('#variables .b')
                                    .assertNumberOfDisplayNodes(1)
                                    .assertNumberOfEditingNodes(1, done)
                            });
                });

    });

});