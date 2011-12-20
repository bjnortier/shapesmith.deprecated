describe("GeomNode", function() {
    
    beforeEach(function() {
	SS.resetGeomCounter();
    });

    it("should be created from args", function() {

        expect(function() {
            new GeomNode({});
        }).toThrow("type is not defined");

        var node = new GeomNode({type: "cuboid", sha: 'b3cf'});
        expect(node.type).toEqual("cuboid");
	expect(node.id).toEqual('1/b3cf');
	expect(node.sha).toEqual('b3cf');

        expect(node.toShallowJson).toBeDefined();
        expect(JSON.parse(node.toShallowJson())).toEqual({type: "cuboid", 
                                                          children: [],
                                                          transforms: []});
    });

    it("can have empty parameters", function() {

        var node = new GeomNode({type: "union", sha: '123'});
        expect(node.type).toEqual("union");
        expect(JSON.parse(node.toShallowJson())).toEqual({type: "union",
                                                          children: [],
                                                          transforms: []});

        expect(JSON.parse(node.toDeepJson())).toEqual({type: "union",
                                                       children: [],
                                                       transforms: []});
    });

    it("can have children", function() {
        
        var child1 = new GeomNode({type: "sphere", sha: '44e', parameters: {x: 1.0}});
        var child2 = new GeomNode({type: "cuboid", sha: '1ba'});
        var parentNode = new GeomNode({type: "union", sha: '0f3'}, [child1, child2]);

        expect(parentNode.children.length).toEqual(2);
        expect(parentNode.children[0]).toEqual(child1);
        expect(parentNode.children[1]).toEqual(child2);

        expect(JSON.parse(parentNode.toShallowJson())).toEqual(
            {type: "union",
             children: ['1/44e', '2/1ba'],
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