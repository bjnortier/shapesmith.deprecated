describe("utils", function() {

    beforeEach(function() {
    });

    it("cap copy an object", function() {

        var obj1 = {a: 1, b:2};
        var copy1 = SS.copyObj(obj1);
        obj1.a = 3;

        expect(copy1).toEqual({a:1, b:2});

        var obj2 = {a:1, b: {x:'x', y:'y'}};
        var copy2 = SS.copyObj(obj2);
        obj2.b.x = '!!';

        expect(copy2).toEqual({a:1, b: {x:'x', y:'y'}});

        var obj3 = {a: [1,2]};
        var copy3 = SS.copyObj(obj3);
        obj3.a.splice(1,1);

        expect(copy3).toEqual({a: [1,2]});
    });
});
