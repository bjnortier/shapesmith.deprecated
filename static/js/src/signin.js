$('#signin-button').click(function() {
    var username = $('#username').val().trim();
    var password = $('#password').val().trim();
    var postJson = {username: username,
		    password: password};
    $.ajax({
        type: 'POST',
	url: '/signin/',
	data: JSON.stringify(postJson),
	dataType: 'json',
	contentType: 'application/json',
	success: function(response) {
	    window.location.href = '/' + username + '/designs';
	},
	error: function(response) {
	    SS.render_errors(JSON.parse(response.responseText));
        }
    });
    return false;
});

