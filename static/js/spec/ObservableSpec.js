describe("Observable", function() {

    var obj;
    beforeEach(function() {
        obj = {};
        Observable.makeObservable(obj);
    });

    it("can receive events", function() {

        // Should receive event
        var received;
        var listener = function(e) {
            received = e;
        };
        obj.addListener(listener);
        var event = {}
        obj.notify(event);
        expect(received).toEqual(event);
        
        // Should no receive the event
        obj.removeListener(listener);
        var anotherEvent = "another event";
        obj.notify(anotherEvent);
        expect(received).toEqual(event);

    });

});
