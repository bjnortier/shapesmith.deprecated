describe("gridextents", function() {

    it("should compute a position inside the X and Y extents", function() {
	
	var gridExtents = SS.gridExtents({minX: -10, minY: -5, maxX: 7, maxY: 3});

	expect(gridExtents.isInsideX(-10.1)).toEqual(false);
	expect(gridExtents.isInsideX(-10)).toEqual(true);
	expect(gridExtents.isInsideX(0)).toEqual(true);
	expect(gridExtents.isInsideX(7)).toEqual(true);
	expect(gridExtents.isInsideX(7.1)).toEqual(false);

	expect(gridExtents.isInsideY(-5.1)).toEqual(false);
	expect(gridExtents.isInsideY(-5)).toEqual(true);
	expect(gridExtents.isInsideY(0)).toEqual(true);
	expect(gridExtents.isInsideY(3)).toEqual(true);
	expect(gridExtents.isInsideY(3.1)).toEqual(false);
	
    });

    
});
