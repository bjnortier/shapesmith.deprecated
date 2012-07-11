var SS = SS || {};

SS.renderDisplayDOM = function(name, schema, object) {
    var nodeDOM = SS.renderRecursiveDisplayDOM(name, schema, object);
    var view = {rows: [name, nodeDOM]};
    return  $.mustache('<table>{{#rows}}<tr><td>{{{.}}}</td></tr>{{/rows}}</table>', view);
}

SS.renderRecursiveDisplayDOM = function(name, schema, object)  {

    if ((schema.type === 'number') || (schema.type === 'string')) {
        return $.mustache('<span class="value">{{value}}</span>', {value: object});

    } else if (schema.type === 'object') {
        var rows = [];
        for (key in schema.properties) {
            rows.push({name: key, dom: SS.renderRecursiveDisplayDOM(key, schema.properties[key], object[key])});
        }
        return $.mustache('<table>{{#rows}}<tr><td>{{name}}</td><td>{{{dom}}}</td></tr>{{/rows}}</table>',
                          {rows: rows});

    } else if (schema.type === 'array') {
        var rows = object.map(function(element) {
            SS.renderRecursiveDisplayDOM(key, schema.items, element);
        });
        return $.mustache('<table>{{#rows}}<tr><td>{{{.}}}</td></tr>{{/rows}}</table>', {rows:rows});
    }
}

SS.renderEditingDOM = function(name, schema, object) {
    var nodeDOM = SS.renderRecursiveEditingDOM([name], name, schema, object);
    var okCancel = '<input class="ok" type="submit" value="Ok"/><input class="cancel" type="submit" value="Cancel"/>';
    var view = {rows: [name, nodeDOM, okCancel]};
    return  $.mustache('<table>{{#rows}}<tr><td>{{{.}}}</td></tr>{{/rows}}</table>', view);
}

SS.renderRecursiveEditingDOM = function(ancestors, name, schema, object)  {
    var tableClass = ancestors.join('_');
    if ((schema.type === 'number') || (schema.type === 'integer')) {
        var template = 
            '<input class="field {{name}}" type="number" value="{{value}}" ' +
            '{{#min}}min="{{.}}"{{/min}} {{#max}}max="{{.}}"{{/max}} />';
        return $.mustache(template, {name: name,
                                     value: object, 
                                     min: schema.minimum, 
                                     max:schema.maximum});

    } else if (schema.type === 'string') {
        if (schema['enum']) {
            var data = {
                name: name,
                options: schema['enum']
            };
            var template = '<select name={{name}}>{{#options}}<option value="{{.}}">{{.}}</option>{{/options}}</select>';
            return $.mustache(template, data);
        } else {
            return $.mustache('<input type="text" value="{{value}}"/>', {value: object});
        }

    } else if (schema.type === 'object') {
        var rows = [];
        for (key in schema.properties) {
            rows.push({name: key, dom: SS.renderRecursiveEditingDOM(ancestors.concat(key), key, schema.properties[key], object[key])});
        }
        return $.mustache(
            '<table class="{{tableClass}}">{{#rows}}<tr><td>{{name}}</td><td>{{{dom}}}</td></tr>{{/rows}}</table>',
            {name:name, tableClass: tableClass, rows:rows});

    } else if (schema.type === 'array') {
        var rows = object.map(function(element, index) {
            return {row: SS.renderRecursiveEditingDOM(ancestors.concat(key), key, schema.items, element),
                    index: index};
        });
        return $.mustache(
            '<table class="{{tableClass}}">{{#rows}}<tr class="{{index}}"><td>{{{row}}}</td></tr>{{/rows}}</table>', 
            {rows:rows, tableClass: tableClass, name:name});
    }
}