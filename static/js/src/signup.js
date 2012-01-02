var SS = SS || {};

SS.render_errors = function(errors) {
    $('.errors').hide();
    for (key in errors) {
	var id = '#' + key + '-errors'
	$(id).css('border', 'solid thin red');
	$(id).text(errors[key]);
	$(id).show();
    }
}

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

$(document).ready(function() {
    var sameSizeFn = function() {
        $('#email-address').width($('#email-address-field').width() - 20);
	$('#email-address').height($('#email-address-button').height());
	$('#email-address-errors').width($('#email-address-field').width() - 20);
    };
    $(window).resize(sameSizeFn);
    sameSizeFn();
});
