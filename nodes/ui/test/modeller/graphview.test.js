var chai = require('chai'),
    assert = chai.assert,
    client = require('./client').client;
chai.Assertion.includeStack = true;

describe('Graph view', function() {

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

    it('will maintain geometry row index on editing', function(done) {
        this.timeout(10000);
        client
            .click('.toolbar .point')
            .waitForUrlChange(
                function() { client.clickOnWorld(0,0,0); },
                function() {
                    client
                        .waitForUrlChange(
                            function() { client.clickOnWorld(20,20,0); },
                            function() {
                                client
                                    .click('.toolbar .select')
                                    .click('#geometry .point0')
                                    .assertTextEqual('#geometry tr:nth-child(1) .title', 'point0')
                                    .assertTextEqual('#geometry tr:nth-child(2) .title', 'point1')
                                    .clickOnWorld(0,0,0)
                                    .click('#geometry .point1')
                                    .assertTextEqual('#geometry tr:nth-child(1) .title', 'point0')
                                    .assertTextEqual('#geometry tr:nth-child(2) .title', 'point1', done);
                            });
    
                });
    });   

    it('will maintain variable row index on editing', function(done) {
        this.timeout(10000);
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
                        .pause(100)
                        .waitForUrlChange(
                            function() { client.clickOnWorld(0,0,0); },
                            function() {
                                client
                                    .click('#variables .a')
                                    .assertValueEqual('#variables tr:nth-child(2) .name .field', 'a')
                                    .assertTextEqual('#variables tr:nth-child(3) .name', 'b')
                                    .pause(100)
                                    .clickOnWorld(0,0,0)
                                    .click('#variables .b')
                                    .assertTextEqual('#variables tr:nth-child(2) .name', 'a')
                                    .assertValueEqual('#variables tr:nth-child(3) .name .field', 'b', done);
                            });
                });
    });               


});