var SS = SS || {};

SS.renderDisplayDOM = function(schema, object)  {

    if ((schema.type === 'number') || (schema.type === 'string')) {
        return '<span class="value">' + object + '</span>';
    } else if (schema.type === 'object') {
        var rows = [];
        for (key in schema.properties) {
            rows.push('<tr><td>' + key + '</td><td>' + 
                      SS.renderDisplayDOM(schema.properties[key], object[key]) + 
                      '</td></tr>');
        }
        return '<table>' + rows.join('') + '</table>';
    } else if (schema.type === 'array') {
        var rows = object.map(function(element) {
            return '<tr><td>' + 
                SS.renderDisplayDOM(schema.items, element) + 
                '</td></tr>';
        });
        return '<table>' + rows.join('') + '</table>';
    }
}

SS.renderEditingDOM = function(schema, object)  {

    if ((schema.type === 'number') || (schema.type === 'integer')) {
        var element = '<input class="field" type="number" value="' + object + '"';
        if (schema.minimum !== undefined) {
            element += ' min="' + schema.minimum + '"';
        }
        if (schema.maximum !== undefined) {
            element += ' max="' + schema.maximum + '"';
        }
        element += '/>';
        return element;
    } else if (schema.type === 'string') {
        if (schema['enum']) {
            var data = {
                options: item['enum']
            };
            var template = '<select>{{#options}}<option value="{{.}}">{{.}}</option>{{/options}}</select>';
            return $.mustache(template, data);
        } else {
            return '<input type="text" value="' + object + '"/>';
        }
    } else if (schema.type === 'object') {
        var rows = [];
        for (key in schema.properties) {
            rows.push('<tr><td>' + key + '</td><td>' + 
                      SS.renderEditingDOM(schema.properties[key], object[key]) + 
                      '</td></tr>');
        }
        return '<table>' + rows.join('') + '</table>';
    } else if (schema.type === 'array') {
        var rows = object.map(function(element) {
            return '<tr><td>' + 
                SS.renderEditingDOM(schema.items, element) + 
                '</td></tr>';
        });
        return '<table>' + rows.join('') + '</table>';
    }
}