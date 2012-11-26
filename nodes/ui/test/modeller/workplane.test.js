var chai = require('chai'),
    assert = chai.assert,
    client = require('./client').client;
chai.Assertion.includeStack = true;

describe('Workplane', function() {

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

    it('can have different snap values', function(done) {
        this.timeout(5000);
        client
            .click('#workplane-settings')
            .clearElement('#workplane-settings .snap')
            .setValue('#workplane-settings .snap', '5')
            .waitForUrlChange(
                function() { client.clickOnWorld(0,0,0); },
                function() {
                    client
                        .click('.toolbar .point')
                        .moveToWorld(3,3,0)
                        .assertCoordinateEqual('.vertex.editing .coordinate', 5, 5, 0, done);
                });
    });

    it('cancels editing when clicking on the workplane', function(done) {
        this.timeout(10000);
        client
            .click('#workplane-settings')
            .assertNumberOfElements('#workplane-settings .vertex.editing', 1)
            .clickOnWorld(0,0,0)
            .assertNumberOfElements('#workplane-settings .vertex.editing', 0, done);
    });

    it.skip('will not change the workplane if there are errors');


});