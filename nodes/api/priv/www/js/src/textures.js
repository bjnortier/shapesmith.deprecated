define(['jquery'], function($) {

    var textures = {};

    $.ajax({
        type: 'GET',
        url: '/materials',
        dataType: 'json',
        success: function(filenames) {
            filenames.forEach(function(filename) {
                THREE.ImageUtils.loadTexture('/materials/' + filename, {},
                    function(texture) {
                        textures[filename] = texture;
                    },
                    function(err) {
                        console.err(err);
                    });
            });
        },
        error: function(msg) {
            console.log(msg);
        }
    });

    return textures;

});