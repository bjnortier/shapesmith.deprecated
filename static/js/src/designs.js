
$('#create-design-button').click(function() {
    var newDesignName = $('#new-name').val();
    $.ajax({
        type: 'POST',
	url: '/' + SS.session.username + '/' + newDesignName + '/',
	data: '{}',
	dataType: 'json',
	contentType: 'application/json',
	success: function(response) {
	    console.log(response);
	    window.location.href = "/{{username}}/" + newDesignName + "/modeller.html?ref=heads.master";
	},
	error: function(response) {
	    $('#new-name').css('background-color', '#ff0000');
	    console.log(response);
        }
    });
});

$(document).ready(function() {
    var sameSizeFn = function() {
        $('#new-name').width($('.design-name').width() - 20);
        $('#new-name').height($('#create-design-button').height());
    };
    $(window).resize(sameSizeFn);
    sameSizeFn();
});