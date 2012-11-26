var remote = require('webdriverjs').remote,
    chai = require('chai'),
    assert = chai.assert;

var client = remote({
    logLevel: 'silent',
    desiredCapabilities:{
        browserName:"chrome",
    }
});

client.addCommand('initDesign', function(callback) {
    this
        .init()
        .url('http://localhost.shapesmith.net:8000/')
        .setValue('#newDesignName', '__test__')
        .click('#create-design-button', function() {
            callback()
        });
});

client.addCommand('freshDesign', function(callback) {
    this    
        .url('http://localhost.shapesmith.net:8000/local/__test__/modeller?commit=fa9d5982d4cec7987eb46383ab6452afcefe85c1&noFadein=true')
        .waitForMainDone(callback);
});

client.addCommand('waitForUrlChange', function(commitFn, callback) {
    client.url(function(result) {
        var from = result.value;
        commitFn();

        var waitForUrlChange = function() {
            var current = client.url(function(result) {
                if (result.value === from) {
                    setTimeout(waitForUrlChange, 100);
                } else {
                    callback();
                }
            })
        }
        waitForUrlChange();
    });
});

client.addCommand('waitForMainDone', function(callback) {
    var waitForDoneFn = function() {
        client.execute('return SS.loadDone', function(result) {
            if (result.value) {
                callback();
            } else {
                setTimeout(waitForDoneFn, 100);
            }
        });
    }
    waitForDoneFn();
});

client.addCommand('reloadCommit', function(callback) {
    this    
        .url(function(result) {
            this
                .url(result.value + '&noFadein=true')
                .waitForMainDone(callback)

        });
});

client.addCommand('assertTextEqual', function(selector, text, callback) {
    this.getText(selector, function(result) {
        assert.equal(result.value, text, selector);
        callback();
    });
});

client.addCommand('assertValueEqual', function(selector, text, callback) {
    this.getValue(selector, function(result) {
        assert.equal(result, text, selector);
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

// Double the moveTo since our drag requires two moves to initiate
client.addCommand('dragToWorld', function(x,y,z, callback) {
    this
        .buttonDown()
        .moveToWorld(x,y,z)
        .moveToWorld(x,y,z)
        .buttonUp(callback)
});

client.addCommand('clickOnWorld', function(x,y,z, callback) {
    this
        .moveToWorld(x,y,z)
        .buttonDown()
        .buttonUp(callback);
});

client.addCommand('dblClickOnWorld', function(x,y,z, callback) {
    this
        .moveToWorld(x,y,z)
        .doDoubleClick(callback)
});

client.addCommand('assertCoordinateEqual', function(selector, x, y, z, callback) {
    this
        .assertValueEqual(selector + ' .x', x)
        .assertValueEqual(selector + ' .y', y)
        .assertValueEqual(selector + ' .z', z, callback);
});

client.addCommand('getEditingVertexName', function(callback) {
    this.getText('.vertex.editing .title .name', function(result) {
        callback(result.value);
    });
});

client.addCommand('assertNumberOfElements', function(selector, expectedLength, callback) {
    this.elements('css selector', selector, function(result) {
        assert.equal(result.value.length, expectedLength);
        callback();
    });
});

client.addCommand('assertNumberOfEditingNodes', function(expectedLength, callback) {
    this.elements('css selector', '#graphs .vertex.editing', function(result) {
        assert.equal(result.value.length, expectedLength);
        callback();
    });
});

client.addCommand('assertNumberOfDisplayNodes', function(expectedLength, callback) {
    this.elements('css selector', '#graphs .vertex.display', function(result) {
        assert.equal(result.value.length, expectedLength);
        callback();
    });
});

exports.client = client;