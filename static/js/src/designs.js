
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

SS.render_design_name_error = function(error) {
    $('#new-name').css('border', 'solid thin red');
    $('#design-name-errors').text(error)
    $('#design-name-errors').show();
}

$('#create-design-button').click(function() {
    var newDesignName = $('#new-name').val().trim();
    if (newDesignName === "") {
	SS.render_design_name_error('please choose a name for the design');
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
	    window.location.href = '/' + SS.session.username + '/' + newDesignName + '/modeller.html?commit=' + response.refs.heads.master;
	},
	error: function(response) {
	    SS.render_design_name_error(JSON.parse(response.responseText));
        }
    });
    return false;
});

$('.load-design').click(function() {
    var designRootUrl = this.href;
    console.info(designRootUrl);
    
    $.ajax({
        type: 'GET',
        url: designRootUrl,
        dataType: 'json',
        success: function(root) { 
	    var commit = root.refs.heads.master;
	    window.location = designRootUrl + 'modeller.html?commit=' + commit;
	},
	error: function(jqXHR, textStatus, errorThrown) {
            console.error(jqXHR.responseText);
	}
    });

    return false;
});


$(document).ready(function() {
    var sameSizeFn = function() {
        $('#new-name').width($('#design-name').width() - 20);
        $('#new-name').height($('#create-design-button').height());
        $('#design-name-errors').width($('#design-name').width() - 20);
    };
    $(window).resize(sameSizeFn);
    sameSizeFn();
    SS.update_empty();
    
});