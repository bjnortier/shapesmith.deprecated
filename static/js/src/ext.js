Function.prototype.method = function (name, func) {
    this.prototype[name] = func;
    return this;
};


String.prototype.weave = function (o) {
    return this.replace(/{([^{}]*)}/g,
        function (a, b) {
            var r = o[b];
            return typeof r === 'string' || typeof r === 'number' ? r : a;
        }
    );
};
