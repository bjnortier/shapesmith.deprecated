define([], function() {

    var classes = document.styleSheets[0].rules || document.styleSheets[0].cssRules;

    function rgbToThreeColor(rgb) {
        var result = /rgb\(([0-9]+),\s*([0-9]+),\s*([0-9.])+\)/.exec(rgb);
        var color = new THREE.Color();
        color.r = parseInt(result[1]);
        color.g = parseInt(result[2]);
        color.b = parseInt(result[3]);
        return color;
    }

    function getColor(selector) {
        for (var x=0; x<classes.length; x++) {
            if (classes[x].selectorText === selector) {
                return rgbToThreeColor(classes[x].style.color);
            }
        }
        throw Error('color ' + selector + ' not in stylesheet(s)');
    }

    return {
        workplane: {
            majorGridLine: getColor('.scene .workplane .majorGridLine'),
            minorGridLine: getColor('.scene .workplane .minorGridLine'),
        }
    }

});