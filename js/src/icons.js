define([
        'requirejsplugins/text!/ui/images/icons/point.svg',
        'requirejsplugins/text!/ui/images/icons/polyline.svg',
        'requirejsplugins/text!/ui/images/icons/cube.svg',
        'requirejsplugins/text!/ui/images/icons/sphere.svg',
        'requirejsplugins/text!/ui/images/icons/subtract.svg',
        'requirejsplugins/text!/ui/images/icons/cog.svg',
        'requirejsplugins/text!/ui/images/icons/tag.svg',
        'requirejsplugins/text!/ui/images/icons/list.svg',
    ], 
    function(
        point, 
        polyline,
        cube,
        sphere,
        subtract,
        cog,
        tag,
        list) {

    return {
        point    : point, 
        polyline : polyline,
        cube     : cube,
        sphere   : sphere,
        subtract : subtract,
        cog      : cog,
        tag      : tag,
        list     : list,
    }

});