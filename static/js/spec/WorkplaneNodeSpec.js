describe("WorkplaneNode", function() {
    
    it("has a default", function() {
        
        var node = new SS.WorkplaneNode();

        expect(node['origin']).toEqual({x:0, y:0, z:0});
        expect(node['u-axis']).toEqual({x:1, y:0, z:0});
        expect(node['w-axis']).toEqual({x:0, y:0, z:1});
        
    });
    
    it("can be constructed", function() {
        
        var node = new SS.WorkplaneNode({origin: {x:0, y:0, z:1}, 
                                         'u-axis': {x: 0, y:1, z:0},
                                         'w-axis': {x: 1, y:0, z:0}
                                        });

        expect(node['origin']).toEqual({x:0, y:0, z:1});
        expect(node['u-axis']).toEqual({x:0, y:1, z:0});
        expect(node['w-axis']).toEqual({x:1, y:0, z:0});

    });

    it("can be copied", function() {
        var node = new SS.WorkplaneNode({origin: {x:0, y:0, z:1}, 
                                         'u-axis': {x: 0, y:1, z:0},
                                         'w-axis': {x: 1, y:0, z:0}
                                        });

        var copy = node.editableCopy();

        expect(copy['origin']).toEqual({x:0, y:0, z:1});
        expect(copy['u-axis']).toEqual({x:0, y:1, z:0});
        expect(copy['w-axis']).toEqual({x:1, y:0, z:0});

    });

});