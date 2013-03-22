define([
        'requirejsplugins/text!/ui/images/icons/point.svg',
        'requirejsplugins/text!/ui/images/icons/polyline.svg',
        'requirejsplugins/text!/ui/images/icons/cube.svg',
        'requirejsplugins/text!/ui/images/icons/sphere.svg',
        'requirejsplugins/text!/ui/images/icons/subtract.svg',
    ], 
    function(
        point, 
        polyline,
        cube,
        sphere,
        subtract) {

    return {
        point    : point, 
        polyline : polyline,
        cube     : cube,
        sphere   : sphere,
        subtract : subtract,
    }

});