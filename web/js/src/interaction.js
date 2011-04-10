var canvas = document.getElementById("theCanvas");

var lastX;
var lastY;
var firstX;
var firstY;

var dragging = false;
var panning = false;
var rotating = false;
var threshhold = 5; // inside this threshold is a single click

function mouseDown(event) {
    firstX = event.clientX;
    firstY = event.clientY;
    lastX = event.clientX;
    lastY = event.clientY
    dragging = true;
}

var shiftPicking = false;
function mouseUp(event) {
    if (!panning && !rotating) {
        /* On mouse down, we render the scene in picking mode, passing in the 
         * mouse canvas coordinates. This will cause a scene render traversal in which
         * all the "picked" listeners will fire on nodes situated above whatever
         * geometry node was picked, as those nodes are visited.
         *
         */
        shiftPicking = event.shiftKey;
        var coords = clickCoordsWithinElement(event);
        picker.beforePick();
        SceneJS.withNode("theScene").pick(coords.x, coords.y);
    }

    dragging = false;
    rotating = false;
    panning = false;
}

function mouseMove(event) {

    if (dragging && !(rotating || panning)) {
        if (event.button == 0) {
            if ((Math.abs(event.clientX - firstX) > threshhold)
                ||
                (Math.abs(event.clientY - firstY) > threshhold)) {

                rotating = true;
            }
        }
        if (event.button == 1) {
            if ((Math.abs(event.clientX - firstX) > threshhold)
                ||
                (Math.abs(event.clientY - firstY) > threshhold)) {
                panning = true;
            }
        }
    }

    if (rotating) {
        sceneView.yaw_angle += (event.clientX - lastX) * 0.5;
        sceneView.pitch_angle -= (event.clientY - lastY) * -0.5;
    }

    if (panning) {
        sceneView.camera_translate.y += (event.clientX - lastX) * 0.01;
        sceneView.camera_translate.z -= (event.clientY - lastY) * 0.01;
    }

    lastX = event.clientX;
    lastY = event.clientY;


}

function cancelEvent(e) {
  e = e ? e : window.event;
  if(e.stopPropagation)
    e.stopPropagation();
  if(e.preventDefault)
    e.preventDefault();
  e.cancelBubble = true;
  e.cancel = true;
  e.returnValue = false;
  return false;
}

function mouseWheel(e) {
    e = e ? e : window.event;
    var raw = e.detail ? e.detail : e.wheelDelta;
    var normal = e.detail ? e.detail * -1 : e.wheelDelta / 40;
    if (sceneView.camera_translate.x + normal < 40) {
        sceneView.camera_translate.x += normal;
    }
    cancelEvent(e);
}

canvas.addEventListener('DOMMouseScroll', mouseWheel, false);  
canvas.addEventListener('mousewheel', mouseWheel, false);

canvas.addEventListener('mousedown', mouseDown, true);
canvas.addEventListener('mousemove', mouseMove, true);
canvas.addEventListener('mouseup', mouseUp, true);

function clickCoordsWithinElement(event) {
    var coords = { x: 0, y: 0};
    if (!event) {
        event = window.event;
        coords.x = event.x;
        coords.y = event.y;
    } else {
        var element = event.target ;
        var totalOffsetLeft = 0;
        var totalOffsetTop = 0 ;

        while (element.offsetParent)
        {
            totalOffsetLeft += element.offsetLeft;
            totalOffsetTop += element.offsetTop;
            element = element.offsetParent;
        }
        coords.x = event.pageX - totalOffsetLeft;
        coords.y = event.pageY - totalOffsetTop;
    }
    return coords;
}