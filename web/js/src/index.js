$(document).ready(function() {
    $.ajax({
        type: 'GET',
        url: '/doc/',
        success: function(ids) {
            var view = {ids : ids};
            var template = '<ul>{{#ids}}<li><a href="/ui.html?docid={{.}}">{{.}}</a></li>{{/ids}}</ul>';
            var existing = $.mustache(template, view);
            $('#existingDocs').append(existing);
        }
    });
});


$('#newButton').click(function() {
    $.ajax({
        type: 'POST',
        url: '/doc/',
        data: '[]',
        dataType: 'json',
        contentType: 'application/json',
        success: function(response) {
            var match = response.path.match('/doc/(.*)');
            var id = match[1];
            window.location.href = "/ui.html?docid=" + id;
        }
    });
});

           