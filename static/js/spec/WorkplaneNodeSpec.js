describe("WorkplaneNode", function() {
    
    it("has a default", function() {
        
        var node = new SS.WorkplaneNode();

        expect(node['origin']).toEqual({x:0, y:0, z:0});
        expect(node['axis']).toEqual({x:0, y:0, z:1});
        expect(node['angle']).toEqual(0);
        
    });
    
    it("can be constructed", function() {
        
        var node = new SS.WorkplaneNode({origin: {x:0, y:0, z:1}, 
                                         'axis': {x: 0, y:1, z:0},
                                         'angle': 30
                                        });

        expect(node['origin']).toEqual({x:0, y:0, z:1});
        expect(node['axis']).toEqual({x:0, y:1, z:0});
        expect(node['angle']).toEqual(30);

    });

    it("can be copied", function() {
        var node = new SS.WorkplaneNode({origin: {x:0, y:0, z:1}, 
                                         'axis': {x: 0, y:1, z:0},
                                         'angle': 45
                                        });

        var copy = node.editableCopy();

        expect(copy['origin']).toEqual({x:0, y:0, z:1});
        expect(copy['axis']).toEqual({x:0, y:1, z:0});
        expect(copy['angle']).toEqual(45);

    });

});