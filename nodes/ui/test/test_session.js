var expect = require('chai').expect,
    webdriverjs = require("webdriverjs"),
    client = webdriverjs.remote({
        logLevel: 'silent',
        desiredCapabilities:{
            browserName:"chrome"
        }
    });

describe('Run Selenium tests', function() {

    before(function(done) {
        client.addCommand('hasText', function(selector, text, callback) {
            this.getText(selector, function(result) {
                expect(result.value).to.have.string(text);
                callback();
            });
        });
        client.addCommand("at", function(url, callback) {
            var that = this;
            function checkAt() {
                that.url(
                    function(result) {
                        if (result.value === url) {
                            callback();
                        } else {
                            setTimeout(checkAt, 500);
                        }
                    }
                );
            }
            checkAt();
        });
        done();
    });

    beforeEach(function(done) {
        this.timeout(5000);
        client.init();
        client.url('http://localhost:8002', done);
    });
    
    it('should redirect to signin', function(done) {
        client.at('http://session.shapesmith.net:8002/ui/signin', function(url) {
            client.hasText('body', 'Sign in to your account.', done);      
        });
    });

    it('can sign up a user', function(done) {
        client.url('http://session.shapesmith.net:8002/ui/signup', function() {
            client
                .hasText('body', 'Create an account in one easy step.')  
                .setValue('#username', 'somedude')
                .setValue('#password1', '123')
                .setValue('#password2', '123')
                .click('#signup-button', function() {
                    client.at('http://session.shapesmith.net:8002/somedude/designs', done);
                });
        });
    });
        
    afterEach(function(done) {
        client.end(done);
    });

});