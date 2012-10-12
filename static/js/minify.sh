#!/usr/bin/env sh
java -jar ~/development/closure-compiler/compiler-latest/compiler.jar lib/csg.js --js_output_file lib/csg-min.js --jscomp_off=internetExplorerChecks --language_in=ECMASCRIPT5
