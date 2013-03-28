define([
        'lib/text!/images/icons/point.svg',
        'lib/text!/images/icons/polyline.svg',
        'lib/text!/images/icons/cube.svg',
        'lib/text!/images/icons/sphere.svg',
        'lib/text!/images/icons/subtract.svg',
        'lib/text!/images/icons/cog.svg',
        'lib/text!/images/icons/tag.svg',
        'lib/text!/images/icons/list.svg',
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