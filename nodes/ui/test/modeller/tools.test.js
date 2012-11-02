var chai = require('chai'),
    assert = chai.assert,
    client = require('./client').client;
chai.Assertion.includeStack = true;

describe('Geometry toolbar', function() {

    before(function(done) {
        client.init(function() {
            done();
        });
    });

    beforeEach(function(done) {
        this.timeout(5000);
        client
            .url('http://localhost.shapesmith.net:8000/local/testdesign/modeller')
            .execute('SS.dontDampTrackball(); SS.zoomIn(); ', function() {
                done();
            })

    });

    after(function(done) {
        client.end(done);
    });

    // ---------- Cases ----------

    it('is on "select" by default', function(done) {
        client.hasClass('.toolbar .select', 'active', done);
    });

    it('has mutually exclusive items', function(done) {
        client
            .click('.toolbar .point')
            .hasClass('.toolbar .point', 'active')
            .doesntHaveClass('.toolbar .select', 'active')
            .click('.toolbar .select')
            .hasClass('.toolbar .select', 'active')
            .doesntHaveClass('.toolbar .point', 'active', done);

    });

});
