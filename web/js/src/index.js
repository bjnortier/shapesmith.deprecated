
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

           