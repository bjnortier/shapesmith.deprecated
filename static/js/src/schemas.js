var SS = SS || {};

SS.schemas = {};
SS.schemas.originSchema =  {
    type: 'object',
    properties: {'x' : {type: 'number'},
                 'y' : {type: 'number'},
                 'z' : {type: 'number'}}
};

SS.schemas.cuboid = {
    description: "Cuboid",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'u' : {type: 'number'},
                'v' : {type: 'number'},
                'w' : {type: 'number'}
                }
        }
    }
};

SS.schemas.sphere = {
    description: "Sphere",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'r' : {type: 'number'}
                }
        }
    }
};

SS.schemas.cylinder = {
    description: "Cylinder",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'r' : {type: 'number'},
                'h' : {type: 'number'}
                }
        }
    }
};

SS.schemas.cone = {
    description: "Cone",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'r1' : {type: 'number'},
                'h' : {type: 'number'},
                'r2' : {type: 'number'}
                }
        }
    }
};

SS.schemas.wedge = {
    description: "Wedge",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'u1' : {type: 'number'},
                'u2' : {type: 'number'},
                'v' : {type: 'number'},
                'w' : {type: 'number'}
                }
        }
    }
};

SS.schemas.torus = {
    description: "Torus",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'r1' : {type: 'number'},
                'r2' : {type: 'number'}
                }
        }
    }
};

SS.schemas.ellipse2d = {
    description: "Ellipse2D",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'r1' : {type: 'number'},
                'r2' : {type: 'number'},
                'from_angle' : {type: 'number'},
                'to_angle' : {type: 'number'}
                }
        }
    }
};

SS.schemas.ellipse1d = {
    description: "Ellipse1D",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'r1' : {type: 'number'},
                'r2' : {type: 'number'},
                'from_angle' : {type: 'number'},
                'to_angle' : {type: 'number'}
                }
        }
    }
};


SS.schemas.rectangle2d = {
    description: "Rectangle2D",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'u' : {type: 'number'},
                'v' : {type: 'number'}
            }
        }
    }
};

SS.schemas.triangle2d = {
    description: "Triangle2D",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'vertices' : {
                    type: 'array',
                    items: { 
                        type: 'object',
                        properties: {
                            'u' : {type: 'number'},
                            'v' : {type: 'number'},
                            'w' : {type: 'number'}
                        }
                    },
                    minItems: 3,
                    maxItems: 3
                }
            }
        }
    }
};

SS.schemas.polyline = {
    description: "Polyline",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'vertices' : {
                    type: 'array',
                    items: { 
                        type: 'object',
                        properties: {
                            'u' : {type: 'number'},
                            'v' : {type: 'number'},
                            'w' : {type: 'number'}
                        }
                    },
                    minItems: 2,
                }
            }
        }
    }
};


SS.schemas.bezier = {
    description: "Bezier",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'vertices' : {
                    type: 'array',
                    items: { 
                        type: 'object',
                        properties: {
                            'u' : {type: 'number'},
                            'v' : {type: 'number'},
                            'w' : {type: 'number'}
                        }
                    },
                    minItems: 4,
                    maxItems: 4
                }
            }
        }
    }
};


SS.schemas.text2d = {
    description: "Text2D",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'font' : {
                    type: 'string',
                    'enum': [ 'OpenSans', 'DroidSerif', 'Inconsolata', 'Lobster']
                },
                'text' : {
                    type: 'string'
                }
            }
        }
    }
};

SS.schemas.prism = {
    description: "Prism",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'u' : {type: 'number'},
                'v' : {type: 'number'},
                'w' : {type: 'number'}
                }
        }
    }
};

SS.schemas.fillet = {
    description: "Fillet",
    type: 'object',
    properties: {
        parameters: {
            type: 'object',
            properties: {
                'r' : {type: 'number'}
            }
        }
    }
};

SS.schemas.revolve = {
    description: "Revolve",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'u' : {type: 'number'},
                'v' : {type: 'number'},
                'w' : {type: 'number'},
                'angle' : {type: 'number'}
                }
        }
    }
};

SS.schemas.intersect = {
    description: "Intersect",
    type: 'object',
    properties: {}
}

SS.schemas.union = {
    description: "Intersect",
    type: 'object',
    properties: {}
}

SS.schemas.subtract = {
    description: "Intersect",
    type: 'object',
    properties: {}
}

SS.schemas.make_face = {
    description: "Make Face",
    type: 'object',
    properties: {}
}

SS.schemas.make_solid = {
    description: "Make Solid",
    type: 'object',
    properties: {}
}

SS.schemas.loft = {
    description: "Loft",
    type: 'object',
    properties: {}
}

SS.schemas.translate = {
    description: "Translate",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'u' : {type: 'number'},
                'v' : {type: 'number'},
                'w' : {type: 'number'},
                'n' : {
                    type: 'integer',
                    minimum: 0
                }
            }
        }
    }
};

SS.schemas.scale = {
    description: "Scale",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'factor' : {type: 'number'}
            }
        }
    }
};

SS.schemas.rotate = {
    description: "Rotate",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'u' : {type: 'number'},
                'v' : {type: 'number'},
                'w' : {type: 'number'},
                'angle' : {type: 'number'},
                'n' : {
                    type: 'integer',
                    minimum: 0
                }
            }
        }
    }
};

SS.schemas.mirror = {
    description: "Mirror",
    type: 'object',
    properties: {
        origin: SS.schemas.originSchema,
        parameters: {
            type: 'object',
            properties: {
                'u' : {type: 'number'},
                'v' : {type: 'number'},
                'w' : {type: 'number'},
                'n' : {
                    type: 'integer',
                    minimum: 0
                }
            }
        }
    }
};

SS.schemas.import_stl = {
    description: "STL",
    type: 'object',
    properties: {
    }
};

SS.schemas.axis_mirror = SS.schemas.mirror;
SS.schemas.plane_mirror = SS.schemas.mirror;
