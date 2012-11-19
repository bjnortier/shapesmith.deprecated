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

    it.skip('will maintain row index on editing', function(done) {
        this.timeout(5000);
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
                                    .assertTextEqual('#geometry tr:nth-child(2) .title', 'point1', done)
                            });
    
            });
               
    });

    it.skip('will display implicit editing rows underneath the parent', function(done) {
        this.timeout(5000);
        client
            .click('.toolbar .polyline')
            .clickOnWorld(0,0,0)
            .clickOnWorld(10,10,0)
            .elements('css selector', '.vertex.editing.polyline0 tr', function(result) {
                assert.equal(result.value.length, 2),
                client
                    .waitForUrlChange(
                        function() { client.dblClickOnWorld(10,10,0); },
                        function() {
                            client
                                .pause(1000)
                                .click('.toolbar .select')
                                .click('.vertex.display.polyline0')
                                .elements('css selector', '.vertex.editing.polyline0 tr', function(result) {
                                    assert.equal(result.value.length, 2);
                                    done();
                                });
                        });
            });


    });


});