
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
	    $('#new-name').css('border', 'solid thin red');
	    console.log(response);
        }
    });
});

SS.delete_design = function(name) {
    var answer = confirm ("This will permanently delete the design. Are you sure?")
    if (answer) {
	$.ajax({
            type: 'DELETE',
	    url: '/' + SS.session.username + '/' + name + '/',
	    success: function(response) {
		$('#design-' + name).remove();
		SS.update_empty();
	    },
	    error: function(response) {
		SS.update_empty();
            }
	});
    }
}

SS.update_empty = function() {
    if ($('.design').length == 0) {
	$('.designs').append('<div class="design empty">nothing yet</div>');
    }    
}

$(document).ready(function() {
    var sameSizeFn = function() {
        $('#new-name').width($('#design-name').width() - 20);
        $('#new-name').height($('#create-design-button').height());
    };
    $(window).resize(sameSizeFn);
    sameSizeFn();
    SS.update_empty();
    
});