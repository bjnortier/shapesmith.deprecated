var SS = SS || {};

SS.renderErrorMessage = function(error) {
    var error;
    try {
	var error = JSON.parse(error);
	$('tr.field').removeClass('validation-error');
	if (error.validation) {
	    for (var i in error.validation) {
		$('#' + i).parents('tr.field').addClass('validation-error');
	    }
	}
    } catch (e) {
	error = {exception: e};
    }

    $('#messages-container').empty();
    if (error.validation) {
	// No need for a message as there is already validation feedback
	console.log(error);
    } else if (error.string) {
	$('#messages-container').append('<div class="error">' + error.string + '</div>');
    } else if (error.error) {
	$('#messages-container').append('<div class="error">' + error.error + '</div>');
    } else {
	$('#messages-container').append('<div class="error">Oops. An unknown problem occurred</div>');
    }
};

SS.clearMessages = function() {
    $('#messages-container').empty();
}

