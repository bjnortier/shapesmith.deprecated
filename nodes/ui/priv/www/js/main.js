requirejs(['src/scene'], function(scene) {

    $(document).ready(function() {

        var container = document.getElementById('scene');
        new scene.Model({container: container});

    });

});