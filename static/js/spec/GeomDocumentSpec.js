describe('GeomDocument', function() {
    
    var doc;
    beforeEach(function() {
        doc = new GeomDocument();
    });

    it('should be empty on creation', function() {
        expect(doc.rootNodes.length).toEqual(0);
    });

    it('can accept and reject root nodes', function() {
        
        var node1 = new GeomNode({type: 'sphere'});
        var node2 = new GeomNode({type: 'cuboid'});
        doc.add(node1);
        doc.add(node2);
        expect(doc.rootNodes.length).toEqual(2);
        expect(doc.rootNodes[1]).toEqual(node1);
        expect(doc.rootNodes[0]).toEqual(node2);

        doc.remove(node1);
        expect(doc.rootNodes.length).toEqual(1);
        expect(doc.rootNodes[0]).toEqual(node2);

        doc.remove(node2);
        expect(doc.rootNodes.length).toEqual(0);
        
    });

    it('can replace nodes', function() {
        var child = new GeomNode({type: 'cuboid', sha: 'a3'});
        var parent = new GeomNode({type: 'cuboid', sha: 'b2'}, [child]);
        var grandparent = new GeomNode({type: 'sphere', sha: 'f1'}, [parent]);

        doc.add(grandparent);

        var child2 = new GeomNode({type: 'cuboid', sha: 'a4'});

        doc.replace(child, child2);
        expect(doc.findBySHA(child2.sha)).toEqual(child2);
        expect(doc.ancestors(child2)).toEqual([parent, grandparent]);
    });
    
    it('can be used to find nodes', function() {
        var node1 = new GeomNode({type: 'sphere', sha: 'a1'});
        var node2 = new GeomNode({type: 'cuboid', sha: 'b2'});
        doc.add(node1);
        doc.add(node2);
        
        expect(doc.findBySHA('a1')).toEqual(node1);
        expect(doc.findBySHA('b2')).toEqual(node2);
    });

    it('can be used to find child nodes', function() {
        var node1 = new GeomNode({type: 'sphere', sha: 'a1'});
        var node2 = new GeomNode({type: 'cuboid', sha: 'b2'}, [node1]);
        doc.add(node2);
        
        expect(doc.findBySHA('a1')).toEqual(node1);
        expect(doc.findBySHA('b2')).toEqual(node2);
    });

    it('can be used to determine the ancestors of a node', function() {
        var child = new GeomNode({type: 'cuboid', path: '/3'});
        var parent = new GeomNode({type: 'cuboid', path: '/2'}, [child]);
        var grandparent = new GeomNode({type: 'sphere', path: '/1'}, [parent]);

        doc.add(grandparent);
        
        expect(doc.ancestors(child)).toEqual([parent, grandparent]);
        expect(doc.ancestors(parent)).toEqual([grandparent]);
        expect(doc.ancestors(grandparent)).toEqual([]);


        expect(function() {
            doc.ancestors(new GeomNode({type: 'cuboid'}));
        }).toThrow("node not found");
    });

    it('can be serialised to json', function() {

        var child = new GeomNode({type: 'cuboid', path: '/3'});
        var parent = new GeomNode({type: 'boolean', path: '/2'}, [child]);
        var grandparent = new GeomNode({type: 'boolean', path: '/1'}, [parent]);
        var another = new GeomNode({type: 'sphere', path: '/4'});
        doc.add(grandparent);
        doc.add(another);

        var serialized = doc.toJson();
        expect(serialized).toEqual([
            { type : 'sphere', children : [], transforms : [] }, 
            { type : 'boolean', children : [ 
                { type : 'boolean', children : [ 
                    { type : 'cuboid', children : [], transforms : [] } ],
                  transforms : [] } ], 
              transforms : [] }
        ]);
    });

});