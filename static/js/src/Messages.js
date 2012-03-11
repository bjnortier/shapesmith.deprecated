var SS = SS || {};

(function() {

    var addMessage = function(message, clazz)  {
	var deleteButton = $('<span class="delete-button"></span>');
	var li = $('<li class="' + clazz + '">' + message + '</li>');
	li.append(deleteButton);
	$('#messages-container ol').append(li);
	deleteButton.click(function() {
	    li.remove();
	});
	var delay = clazz === 'info' ? 2000 : 4000;
	li.delay(delay).fadeOut(500, function() { li.remove() });
    }

    SS.renderInfoMessage = function(message) {
	addMessage(message, 'info');
    }

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

	if (error.validation) {
            // Show is tree view is not visible
            if ($('#geom-model-doc :visible').length === 0) {
                addMessage('Validation failure. Check the parameters in the tree view', 'error');
            }
	    console.log(error);
	} else if (error.string) {
	    addMessage(error.string, 'error');
	} else if (error.error) {
	    addMessage(error.error, 'error');
	} else {
	    addMessage('Sorry. An unknown problem occurred', 'error');
	}
};
})();


