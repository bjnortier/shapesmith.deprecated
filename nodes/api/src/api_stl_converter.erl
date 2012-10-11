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

%% 
%% Converts an ASCII or binary STL file into a set of normals and vertices
%%
-module(api_stl_converter).
-author('Benjamin Nortier <bjnortier@gmail.com>').
-export([convert/1]).

convert(Contents) when is_binary(Contents) ->
    case is_ascii(Contents) of
        true ->
            convert_ascii(Contents);
        false ->
            convert_binary(Contents)
    end;
convert(_) ->
    throw(binary_contents_expected).

is_ascii(_Contents) ->
    true.

convert_ascii(Contents) ->
    ascii_fsm(start, 
              lists:map(fun binary_to_list/1, 
                        re:split(Contents, "\\r{0,1}\\n"))).

convert_binary(_Contents) ->
    [{normals, [],
      vertices, []}].


ascii_fsm(start, ["solid " ++ _|Rest]) ->
    ascii_fsm(facet_normal, Rest, [], []).

ascii_fsm(facet_normal, [Line|Rest], Normals, Triangles) ->
    ascii_fsm(outer_loop, Rest, Normals ++ [to_normal(Line)], Triangles);
ascii_fsm(outer_loop, [_|Rest], Normals, Triangles) ->
    ascii_fsm(v1, Rest, Normals, Triangles, {});
ascii_fsm(endloop, [_|Rest], Normals, Triangles) ->
    ascii_fsm(endfacet, Rest, Normals, Triangles);
ascii_fsm(endfacet, [_Line|Rest], Normals, Triangles) ->
    [NextLine|_] = Rest,
    case re:run(NextLine, "\\s*facet") of
        {match, _} ->
            ascii_fsm(facet_normal, Rest, Normals, Triangles);
        _ ->
            ascii_fsm(endsolid, Rest, Normals, Triangles)
    end;
ascii_fsm(endsolid, _, Normals, Triangles) ->
    [{normals, Normals},
     {triangles, Triangles}].

ascii_fsm(v1, [Line|Rest], Normals, Triangles, {}) ->
    ascii_fsm(v2, Rest, Normals, Triangles, {to_vertex(Line)});
ascii_fsm(v2, [Line|Rest], Normals, Triangles, {V1}) ->
    ascii_fsm(v3, Rest, Normals, Triangles, {V1, to_vertex(Line)});
ascii_fsm(v3, [Line|Rest], Normals, Triangles, {V1, V2}) ->
    ascii_fsm(endloop, Rest, Normals, Triangles ++ [{V1, V2, to_vertex(Line)}]).


to_normal(Line) ->
    to_vector(Line, "facet normal").

to_vertex(Line) ->
    to_vector(Line, "vertex").
    
to_vector(Line, Prefix) ->
    {match,[_,{X1,X2},{Y1,Y2},{Z1,Z2}]} =
        re:run(Line,"\\s*" ++ Prefix ++ "\\s+([-+.e0-9]+)\\s+([-+.e0-9]+)\\s+([-+.e0-9]+)"),
    {to_float(Line, X1, X2), to_float(Line, Y1, Y2), to_float(Line, Z1,Z2)}.

to_float(Line, Start, Length) ->
    list_to_float(string:substr(Line, Start + 1, Length)).
    
