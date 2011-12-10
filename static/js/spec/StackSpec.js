describe("Stack", function() {

    var stack;
    beforeEach(function() {
        stack = new Stack();
    });

    it("can be pushed and popped", function() {
	stack.push(1);
	stack.push(2);
	
	expect(stack.peek()).toEqual(2);
	expect(stack.pop()).toEqual(2);

	expect(stack.peek()).toEqual(1);
	expect(stack.pop()).toEqual(1);

	expect(stack.peek()).toEqual(undefined);

	expect(function() {
	    stack.pop();
	}).toThrow("empty");
    });

});
