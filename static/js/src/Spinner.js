var SS = SS || {};
SS.Spinner = function() {

    var stackSize = 0;

    this.show = function() {
	if (stackSize == 0) {
	    if ($('#progress-container').children().size() == 0) {
		$('#progress-container').append(
		    '<div id="progress"><img src="/static/images/progress-spinner.gif" alt="in progress"/></div>');
	    }
	    $('#progress').show();
	}
	++stackSize;
    }

    this.hide = function() {
	--stackSize;
	if (stackSize == 0) {
	    $('#progress').hide();
	}
    }
    
}
SS.spinner = new SS.Spinner();
SS.withSpinner = function(closure) {
    SS.spinner.show();
    try {
	closure();
    } catch (e) {
	console.error(e);
    }
    SS.spinner.hide();
}
