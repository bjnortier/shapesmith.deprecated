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
        // Treeview error display must be updated for th enew tree view
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
            var strings = [];
            for (key in error.validation) {
                strings.push(key + ':' + error.validation[key]);
            }
            addMessage(strings.join(', '), 'error');
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


