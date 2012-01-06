$('#signup-button').click(function() {
    var username = $('#username').val().trim();
    var emailAddress = $('#emailAddress').val().trim();
    var password1 = $('#password1').val().trim();
    var password2 = $('#password2').val().trim();
    var postJson = {username: username,
		    emailAddress: emailAddress,
		    password1: password1,
		    password2: password2};
    $.ajax({
        type: 'POST',
	url: '/signup/',
	data: JSON.stringify(postJson),
	dataType: 'json',
	contentType: 'application/json',
	success: function(response) {
	    $('#email-address').css('border', 'none');
	    $('#email-address-errors').hide();
	    console.log(response);
	},
	error: function(response) {
	    SS.render_errors(JSON.parse(response.responseText));
        }
    });
    return false;
});

