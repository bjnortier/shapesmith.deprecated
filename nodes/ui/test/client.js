var remote = require('webdriverjs').remote,
    chai = require('chai'),
    assert = chai.assert;

var client = remote({
    logLevel: 'silent',
    desiredCapabilities:{
        browserName:"chrome",
    }
});

client.addCommand('assertTextEqual', function(selector, text, callback) {
    this.getText(selector, function(result) {
        assert.equal(text, result.value, selector);
        callback();
    });
});

client.addCommand('hasClass', function(selector, clazz, callback) {
    this.getAttribute(selector, 'class', function(result) {
        assert.include(result, clazz);
        callback();
    });
});

client.addCommand('doesntHaveClass', function(selector, clazz, callback) {
    this.getAttribute(selector, 'class', function(result) {
        assert.isTrue(result.indexOf(clazz) === -1);
        callback();
    });
});

client.addCommand('moveToWorld', function(x,y,z, callback) {
    this.execute('return SS.toScreenCoordinates('+x+','+y+','+z+')', function(result) {
        var xOffset = Math.round(result.value.x);
        var yOffset = Math.round(result.value.y);
        client.element('css selector', '#scene', function(result) {
            var element = result.value.ELEMENT;
            client
                .moveTo(element, xOffset, yOffset, callback)
        });
    });
});

client.addCommand('clickOnWorld', function(x,y,z, callback) {
    this
        .moveToWorld(x,y,z)
        .buttonDown()
        .buttonUp(callback);
});

client.addCommand('assertCoordinateEqual', function(selector, x, y, z, callback) {
    this.assertTextEqual(selector + ' .x', x);
    this.assertTextEqual(selector + ' .y', y);
    this.assertTextEqual(selector + ' .z', z);
    client.pause(0, callback);
});

client.addCommand('getEditingVertexName', function(callback) {
    this.getText('.vertex.editing .title .name', function(result) {
        callback(result.value);
    });
});

exports.client = client;