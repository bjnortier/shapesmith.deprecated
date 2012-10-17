var SS = SS || {};
requirejs(['src/scene'], function(scene) {

    $(document).ready(function() {

        var container = document.getElementById('scene');
        SS.sceneModel = new scene.Model({container: container});

    });

});