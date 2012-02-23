
SS.load_design = function(username, name) {
    var designUrl = '/' + username + '/' + name + '/';
    $.ajax({
        type: 'GET',
        url: designUrl,
        dataType: 'json',
        success: function(root) { 
	    var commit = root.refs.heads.master;
	    window.location = designUrl + 'modeller?commit=' + commit;
	},
	error: function(jqXHR, textStatus, errorThrown) {
            console.error(jqXHR.responseText);
	}
    });

    return false;
}

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

$('#create-design-button').click(function() {
    var newDesignName = $('#newDesignName').val().trim();
    if (newDesignName === "") {
	SS.render_errors({newDesignName: 'please choose a name for the design'});
	return false;
    }
    $.ajax({
        type: 'POST',
	url: '/' + SS.session.username + '/' + newDesignName + '/',
	data: '{}',
	dataType: 'json',
	contentType: 'application/json',
	success: function(response) {
	    console.log(response);
	    window.location.href = '/' + SS.session.username + '/' + newDesignName + '/modeller?commit=' + response.refs.heads.master;
	},
	error: function(response) {
	    SS.render_errors(JSON.parse(response.responseText));
        }
    });
    return false;
});



$(document).ready(function() {
    SS.update_empty();
});
