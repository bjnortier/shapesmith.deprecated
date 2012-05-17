%% -*- mode: erlang -*-
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% Copyright 2011 Benjamin Nortier
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.

-module(api_stl_converter_tests).
-author('Benjamin Nortier <bjnortier@shapesmith.net>').
-include_lib("eunit/include/eunit.hrl").

ascii_stl_file_test_() ->
    AsciiFile1LF = <<"solid scratch\n facet normal -1.000000e+00 -0.000000e+00 -0.000000e+00\n   outer loop\n     vertex  0.000000e+00  1.000000e+01  1.000000e+01\n     vertex  0.000000e+00  1.000000e+01  0.000000e+00\n     vertex  0.000000e+00  0.000000e+00  1.000000e+01\n   endloop\n endfacet\nendsolid scratch">>,
    AsciiFile1CRLF = <<"solid scratch\n\r facet normal -1.000000e+00 -0.000000e+00 -0.000000e+00\n\r   outer loop\n\r     vertex  0.000000e+00  1.000000e+01  1.000000e+01\n\r     vertex  0.000000e+00  1.000000e+01  0.000000e+00\n\r     vertex  0.000000e+00  0.000000e+00  1.000000e+01\n\r   endloop\n\r endfacet\n\rendsolid scratch">>,
    Result1 = [{normals, [{-1.0, 0.0, 0.0}]},
              {triangles, [{{0.0, 10.0, 10.0}, {0.0, 10.0, 0.0}, {0.0, 0.0, 10.0}}]}],
    
    AsciiFile2 = <<"solid scratch\n facet normal -1.000000e+00 -0.000000e+00 -0.000000e+00\n   outer loop\n     vertex  0.000000e+00  1.000000e+01  1.000000e+01\n     vertex  0.000000e+00  1.000000e+01  0.000000e+00\n     vertex  0.000000e+00  0.000000e+00  1.000000e+01\n   endloop\n endfacet\n facet normal -4.000000e+00 -0.000000e+00 -0.600000e+00\n   outer loop\n     vertex  7.000000e+00  1.000000e+01  1.000000e+01\n     vertex  0.000000e+00  1.000000e+01  5.000000e+00\n     vertex  1.000000e+00  0.000000e+00  1.000000e+01\n   endloop\n endfacet\nendsolid scratch">>,
    Result2 = [{normals, [{-1.0, 0.0, 0.0}, {-4.0, 0.0, -0.6}]},
               {triangles, [{{0.0, 10.0, 10.0}, {0.0, 10.0, 0.0}, {0.0, 0.0, 10.0}},
                            {{7.0, 10.0, 10.0}, {0.0, 10.0, 5.0}, {1.0, 0.0, 10.0}}]}],
    [
     ?_assertEqual(Result1, api_stl_converter:convert(AsciiFile1LF)),
     ?_assertEqual(Result1, api_stl_converter:convert(AsciiFile1CRLF)),
     ?_assertEqual(Result2, api_stl_converter:convert(AsciiFile2))
    ].
