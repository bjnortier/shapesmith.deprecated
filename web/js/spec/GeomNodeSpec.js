describe("GeomNode", function() {
    
    beforeEach(function() {
    });

    it("should be created from args", function() {

        expect(function() {
            new GeomNode({});
        }).toThrow("type is not defined");

        var node = new GeomNode({type: "cuboid"});
        expect(node.type).toEqual("cuboid");

        expect(node.toShallowJson).toBeDefined();
        expect(JSON.parse(node.toShallowJson())).toEqual({type: "cuboid", 
                                                          children: [],
                                                          transforms: []});
    });

    it("can have empty parameters", function() {

        var node = new GeomNode({type: "union"});
        expect(node.type).toEqual("union");
        expect(JSON.parse(node.toShallowJson())).toEqual({type: "union",
                                                          children: [],
                                                          transforms: []});

        expect(JSON.parse(node.toDeepJson())).toEqual({type: "union",
                                                       children: [],
                                                       transforms: []});
    });

    it("can have children", function() {
        
        var child1 = new GeomNode({type: "sphere", path: '/1', parameters: {x: 1.0}});
        var child2 = new GeomNode({type: "cuboid", path: '/2'});
        var parentNode = new GeomNode({type: "union", path: '/3'}, [child1, child2]);

        expect(parentNode.children.length).toEqual(2);
        expect(parentNode.children[0]).toEqual(child1);
        expect(parentNode.children[1]).toEqual(child2);

        expect(JSON.parse(parentNode.toShallowJson())).toEqual(
            {type: "union",
             children: ['/1', '/2'],
             transforms: []});
        
        expect(JSON.parse(parentNode.toDeepJson())).toEqual(
            {type: "union",
             children: [
                 {type: 'sphere',
                  children: [],
                  transforms: [],
                  parameters: {x: 1.0}},
                 {type: 'cuboid',
                  children: [],
                  transforms: []}
             ],
             transforms: []});


    });

    



});