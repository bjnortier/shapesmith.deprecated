$('#signup-button').click(function() {
    var username = $('#username').val().trim();
    var emailAddress = $('#emailAddress').val().trim();
    var password1 = $('#password1').val().trim();
    var password2 = $('#password2').val().trim();
    var postJson = {username: username,
		    password1: password1,
		    password2: password2};
    if (emailAddress) {
	postJson.emailAddress = emailAddress;
    }
    $('#spinner-container').append('<div id="spinner"><img src="/static/images/progress-spinner.gif" alt="in progress"/></div>');
    $.ajax({
        type: 'POST',
	url: '/signup/',
	data: JSON.stringify(postJson),
	dataType: 'json',
	contentType: 'application/json',
	success: function(response) {
	    window.location.href = '/' + username + '/designs';
	},
	error: function(response) {
	    SS.render_errors(JSON.parse(response.responseText));
	    $('#spinner').remove();
        }
    });
    return false;
});

