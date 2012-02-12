var Observable = {};

Observable.addListener = function(listener) {
    this.listeners.push(listener);
};

Observable.removeListener = function(listener) {
    var index = this.listeners.indexOf(listener);
    if (index !== -1) {
        this.listeners.splice(index, 1);
    }
};

Observable.notify = function(event) {
    for (var i in this.listeners) {
        this.listeners[i](event);
    }
};


Observable.makeObservable = function(obj) {
    obj.listeners = [];
    obj.addListener = Observable.addListener;
    obj.removeListener = Observable.removeListener;
    obj.notify = Observable.notify;
}



