function SceneView() {

    this.eye = {x: 50.0, y: 0.0, z: 0.0};
    this.look = {x: 0.0, y:0.0, z:0.0};
    this.up = {z: 1.0};

    this.yaw_angle = -45;
    this.pitch_angle = 20;
    this.camera_translate = {x: 0, y:0, z:0};

    var add = function(geomNode) {
        if (geom_doc.isRoot(geomNode) && geomNode.mesh) {
            var sceneNode = {type : "geometry",
                             indices : geomNode.mesh.indices,
                             positions : geomNode.mesh.positions,
                             normals : geomNode.mesh.normals,
                             primitive : geomNode.mesh.primitive};

            SceneJS.withNode("geom").add("node", {type: "material",
                                                  id: geomNode.path,
                                                  emit: 0,
                                                  baseColor:      { r: 0.5, g: 1.0, b: 0.0 },
                                                  specularColor:  { r: 0.9, g: 0.9, b: 0.9 },
                                                  specular:       0.9,
                                                  shine:          100.0,
                                                  nodes: [sceneNode]});
            
            picker.addPickable(geomNode.path);
        }
    }

    var remove = function(geomNode) {
        if (geomNode.path 
            && 
            SceneJS.nodeExists(geomNode.path) 
            && 
            SceneJS.withNode(geomNode.path).parent()) {

            SceneJS.withNode(geomNode.path).parent().remove({node: geomNode.path});
        }
    }

    this.selectionUpdated = function(event) {
        if (event.deselected) {
            for (var i in event.deselected) {
                var path = event.deselected[i];
                SceneJS.withNode(path).set('baseColor', { r: 0.5, g: 1.0, b: 0.0 });
            }
        }
        if (event.selected) {
            for (var i in event.selected) {
                var path = event.selected[i];
                SceneJS.withNode(path).set('baseColor', { r: 1.0, g: 1.0, b: 0.0 });
            }
        }
    }

    this.geomDocUpdated = function(event) {

        if (event.add) {
            add(event.add);
        }

        if (event.remove) {
            remove(event.remove);
        }

        if (event.replace) {
            remove(event.replace.original);
            add(event.replace.replacement);
        }


    }

}

var sceneView = new SceneView();

var scene = new SceneJS.Scene(
    {
        id: "theScene",
        canvasId: "theCanvas",
        loggingElementId: "theLoggingDiv"
    },
    new SceneJS.LookAt(
        {
            id : "lookat",
            eye : sceneView.eye,
            look : sceneView.look,
            up : sceneView.up
        },
        new SceneJS.Camera(
            {
                type: "camera",
                optics: {
                    type: "perspective",
                    fovy : 5.0,
                    aspect : 1.47,
                    near : 0.10,
                    far : 3000.0
                }
            },
            new SceneJS.Light(
                {
                    mode:                   "dir",
                    color:                  { r: 0.5, g: 0.5, b: 0.5 },
                    diffuse:                true,
                    specular:               true,
                    dir:                    { x: -1.0, y: -1.0, z: 1.0 }
                }),
            new SceneJS.Light(
                {
                    mode:                   "dir",
                    color:                  { r: 0.7, g: 0.7, b: 0.7 },
                    diffuse:                true,
                    specular:               true,
                    dir:                    { x: 0.0, y: 1.0, z: -1.0 }
                }),
            new SceneJS.Light(
                {
                    mode:                   "dir",
                    color:                  { r: 0.8, g: 0.8, b: 0.8 },
                    diffuse:                true,
                    specular:               true,
                    dir:                    { x: -1.0, y: -1.0, z: 1.0 }
                }),
            new SceneJS.Translate(
                {
                    id : "cameraTranslate",
                    x : sceneView.camera_translate.x,
                    y : sceneView.camera_translate.y,
                    z : sceneView.camera_translate.z
                },
                new SceneJS.Rotate(
                    {
                        id: "pitch",
                        angle: sceneView.pitch_angle,
                        y : 1.0
                    },
                    new SceneJS.Rotate(
                        {
                            id: "yaw",
                            angle: sceneView.yaw_angle,
                            z : 1.0
                        },
                        new SceneJS.Material(
                            {
                                id: "x-axis",
                                emit: 1,
                                baseColor:      { b: 1.0 },
                                specular:       0,
                            },
                            new SceneJS.Geometry(
                                {
                                    primitive: "lines",
                                    positions : [
                                        0,0,0,
                                        1000,0,0
                                    ],
                                    normals : [],
                                    uv : [],
                                    uv2 : [],
                                    indices : [0,1]
                                })),
                        new SceneJS.Material(
                            {
                                id: "y-axis",
                                emit: 1,
                                baseColor:      { g: 1.0 },
                                specular:       0,
                            },
                            new SceneJS.Geometry(
                                {
                                    primitive: "lines",
                                    positions : [
                                        0,0,0,
                                        0,1000,0
                                    ],
                                    normals : [],
                                    uv : [],
                                    uv2 : [],
                                    indices : [0,1]
                                })),
                        new SceneJS.Material(
                            {
                                id: "z-axis",
                                emit: 1,
                                baseColor:      { r: 1.0 },
                                specular:       0,
                            },
                            new SceneJS.Geometry(
                                {
                                    primitive: "lines",
                                    positions : [
                                        0,0,0,
                                        0,0,1000
                                    ],
                                    normals : [],
                                    uv : [],
                                    uv2 : [],
                                    indices : [0,1]
                                })),
                        new SceneJS.Material(
                            {
                                id: "geom",
                                emit: 0,
                                baseColor:      { r: 0.5, g: 1.0, b: 0.0 },
                                specularColor:  { r: 0.9, g: 0.9, b: 0.9 },
                                specular:       0.9,
                                shine:          100.0,
                            }
                        )
                    )
                )
            )
        )
    )
);
              

window.render = function() {
    
    SceneJS.withNode("pitch").set("angle", sceneView.pitch_angle);
    SceneJS.withNode("yaw").set("angle", sceneView.yaw_angle);
    SceneJS.withNode("cameraTranslate").set("x", sceneView.camera_translate.x);
    SceneJS.withNode("cameraTranslate").set("y", sceneView.camera_translate.y);
    SceneJS.withNode("cameraTranslate").set("z", sceneView.camera_translate.z);

    SceneJS.withNode("theScene").render();
};


/* Render loop until error or reset
 * (which IDE does whenever you hit that run again button)
 */
var pInterval;
SceneJS.bind("error", function() {
    window.clearInterval(pInterval);
});
SceneJS.bind("reset", function() {
    window.clearInterval(pInterval);
});
pInterval = window.setInterval("window.render()", 10);







