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

});