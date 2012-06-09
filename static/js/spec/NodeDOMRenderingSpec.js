describe("NodeDOMRenderer", function() {

    it("can render a number", function() {

        var schema = {
            type: 'number',
        };
        var html = SS.renderDisplayDOM(schema, 1);
        
	expect(html).toEqual('1');

    });

    it("can render a flat schema", function() {

        var schema = {
            type: 'object',
            properties: {'x' : {type: 'number'},
                         'y' : {type: 'number'},
                         'z' : {type: 'number'}}
        };
        var obj = {x: 1, y:2, z:3};
        var html = SS.renderDisplayDOM(schema, obj);
        
	expect(html).toEqual('<table><tr><td>x</td><td>1</td></tr><tr><td>y</td><td>2</td></tr><tr><td>z</td><td>3</td></tr></table>');
    });

    it("can render a simple nested schema", function() {

        var schema = {
            type: 'object',
            properties: {
                origin:  {
                    type: 'object',
                    properties: {'x' : {type: 'number'},
                                 'y' : {type: 'number'},
                                 'z' : {type: 'number'}}
                }
            }
        };
        var obj = {origin: {x: 1, y:2, z:3}};
        var html = SS.renderDisplayDOM(schema, obj);
        
	expect(html).toEqual('<table><tr><td>origin</td><td><table><tr><td>x</td><td>1</td></tr><tr><td>y</td><td>2</td></tr><tr><td>z</td><td>3</td></tr></table></td></tr></table>');
    });

    it("can render an simple array", function() {

        var schema = {
            type: 'array',
            items: { 
                type: 'object',
                properties: {
                    'a' : {type: 'number'},
                    'b' : {type: 'number'}
                }
            },
        }
        var array = [{a:1, b:2}, {a:5, b:6}];
        var html = SS.renderDisplayDOM(schema, array);

        expect(html).toEqual('<table><tr><td>' +
                             '<table>' + 
                             '<tr><td>a</td><td>1</td></tr>' + 
                             '<tr><td>b</td><td>2</td></tr>' +
                             '</table>' + 
                             '</td></tr><tr><td>' + 
                             '<table>' + 
                             '<tr><td>a</td><td>5</td></tr>' + 
                             '<tr><td>b</td><td>6</td></tr>' +
                             '</table>' + 
                             '</td></tr></table>')
    });
        
});
