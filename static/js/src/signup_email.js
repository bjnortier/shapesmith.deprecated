var SS = SS || {};

SS.render_email_address_error = function(error) {
    $('#email-address').css('border', 'solid thin red');
    $('#email-address-errors').text(error)
    $('#email-address-errors').show();
}

$('#email-address-button').click(function() {
    var emailAddress = $('#email-address').val().trim();
    if (emailAddress === "") {
	SS.render_email_address_error('please provide your email address');
	return false;
    }
    $.ajax({
        type: 'POST',
	url: '/signup_email/',
	data: JSON.stringify(emailAddress),
	dataType: 'json',
	contentType: 'application/json',
	success: function(response) {
	    $('#email-address').css('border', 'none');
	    $('#email-address-errors').hide();
	    console.log(response);
	},
	error: function(response) {
	    SS.render_email_address_error(JSON.parse(response.responseText));
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
