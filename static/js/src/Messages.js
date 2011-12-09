function renderErrorMessage(error) {
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
}

function renderSuccessMessage(message) {
    $('#messages-container').empty();
    $('#messages-container').append('<div class="info">' + message + '</div>');
}

function clearMessages() {
    $('#messages-container').empty();
}


function showSpinner() {
    if ($('#progress-container').children().size() == 0) {
        $('#progress-container').append(
	    '<div id="progress"><img src="images/progress-spinner.gif" alt="in progress"/></div>');
    }
    $('#progress').show();
}

function hideSpinner() {
    $('#progress').hide();
}