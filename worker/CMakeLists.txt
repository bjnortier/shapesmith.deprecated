cmake_minimum_required (VERSION 2.6)
project (worker)

# The version number.
set (worker_VERSION_MAJOR 0)
set (worker_VERSION_MINOR 1)

# configure a header file to pass some of the CMake settings
# to the source code
configure_file (
  "${PROJECT_SOURCE_DIR}/WorkerConfig.h.in"
  "${PROJECT_BINARY_DIR}/WorkerConfig.h"
  )

# add the binary tree to the search path for include files
# so that we will find TutorialConfig.h
include_directories("${PROJECT_BINARY_DIR}")
include_directories("include")
include_directories("/usr/local/include")

 
# add the executable
add_executable(worker src/worker.cxx src/base64.cpp)

include_directories ("/Library/OpenCASCADE/6.3.0/inc")

set(CMAKE_CXX_FLAGS "-DHAVE_CONFIG_H")

target_link_libraries(worker -L/Library/OpenCASCADE/6.3.0/lib -ljson_spirit -lpthread -lTKernel -lPTKernel -lTKMath -lTKService -lTKV3d -lTKV2d -lTKBRep -lTKIGES -lTKSTL -lTKVRML -lTKSTEP -lTKSTEPAttr -lTKSTEP209 -lTKSTEPBase -lTKShapeSchema -lTKGeomBase -lTKGeomAlgo -lTKG3d -lTKG2d -lTKXSBase -lTKPShape -lTKShHealing -lTKHLR -lTKTopAlgo -lTKMesh -lTKPrim -lTKCDF -lTKBool -lTKBO -lTKFillet -lTKOffset)


