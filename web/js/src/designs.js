$('#create-button').click(function() {
    var newDesignName = $('#new-design-name').val();
    $.ajax({
	type: 'POST',
	url: '/bjnortier/' + newDesignName + '/',
	data: '{}',
	dataType: 'json',
	contentType: 'application/json',
	success: function(response) {
	    console.log(response);
	    //window.location.href = "/web/ui.html?username=bjnortier&design=" + newDesignName;
	},
	error: function(response) {
	    console.log(response);
	}
    });
});


$(document).ready(function() {
    var sameSizeFn = function() {
	$('#new-name').width($('.design-name').width() - 20);
	$('#new-name').height($('#create-design-button').height());
	console.log($('#create-design-button').width());
    };
    $(window).resize(sameSizeFn);
    sameSizeFn();
});