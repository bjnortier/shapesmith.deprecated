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
	expect(node.id).toEqual('1_b3cf');
	expect(node.sha).toEqual('b3cf');

        expect(node.toShallowJson).toBeDefined();
        expect(JSON.parse(node.toShallowJson())).toEqual({type: "cuboid", 
                                                          children: [],
                                                          transforms: []});
    });


    it("can have its SHA updated", function() {

        var node = new GeomNode({type: "cuboid", sha: 'b3cf'});
	expect(node.sha).toEqual('b3cf');

	node.setSHA('a2f1');
	expect(node.sha).toEqual('a2f1');
	

    });

    it("can have empty parameters", function() {

        var node = new GeomNode({type: "union", sha: '123'});
        expect(node.type).toEqual("union");
        expect(JSON.parse(node.toShallowJson())).toEqual({type: "union",
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
             children: ['44e', '1ba'],
             transforms: []});

    });

    it("can be deserialised", function() {
	var child1 = new GeomNode({type: "sphere", sha: '44e', parameters: {x: 1.0}});
        var child2 = new GeomNode({type: "cuboid", sha: '1ba'});
        var parentNode = new GeomNode({type: "union", sha: '0f3'}, [child1, child2]);
	
	var child1FromJson = GeomNode.fromDeepJson(
	    {sha: '44e',
	     geometry: {type: 'sphere',
			parameters: {x:1.0}}});
	expect(child1FromJson.sha).toEqual('44e');
	expect(child1FromJson.type).toEqual(child1.type);
	expect(child1FromJson.parameters).toEqual(child1.parameters);

	var parentFromJson = GeomNode.fromDeepJson(
	    {sha: '0f3',
	     geometry: {type: 'union',
			children: [{sha: '44e',
				    geometry: {type: 'sphere',
					       parameters: {x:1.0}}},
				   {sha: '1ba',
				    geometry: {type: 'cuboid'}}
				  ]}});
	expect(parentFromJson.sha).toEqual('0f3');
	expect(parentFromJson.children[0].sha).toEqual('44e');
	expect(parentFromJson.children[0].type).toEqual(child1.type);
	expect(parentFromJson.children[0].parameters).toEqual(child1.parameters);
	expect(parentFromJson.children[1].sha).toEqual('1ba');
	expect(parentFromJson.children[1].type).toEqual(child2.type);



    });
    



});