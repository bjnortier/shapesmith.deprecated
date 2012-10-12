-module(api_gts_utils).
-export([json_to_gts/1, json_to_stl/1]).

json_to_gts({Props}) ->
    {_, {FaceProps}} = lists:keyfind(<<"faces">>, 1, Props),
    {_, Positions} = lists:keyfind(<<"positions">>, 1, FaceProps),
    Vertices = lists:reverse(
                 lists:foldl(fun(Value, []) ->
                                     [{Value}];
                                (Value, [{X}|Rest]) ->
                                     [{X, Value}|Rest];
                                (Value, [{X,Y}|Rest]) ->
                                     [{X,Y,Value}|Rest];
                                (Value, Acc = [{_X,_Y,_Z}|_Rest]) ->
                                     [{Value}|Acc]
                             end,
                             [],
                             Positions)),
    {_, Indices} =  lists:keyfind(<<"indices">>, 1, FaceProps),
    IndexTriplets = lists:foldl(fun(Value, []) ->
                                        [{Value}];
                                   (Value, [{A}|Rest]) ->
                                        [{A, Value}|Rest];
                                   (C, [{A,B}|Rest]) ->
                                        [{A+1,B+1,C+1}|Rest];
                                   (Value, Acc = [{_A,_B,_C}|_Rest]) ->
                                        [{Value}|Acc]
                                end,
                                [],
                                Indices),
    EdgeSet = lists:foldl(fun({A,B,C}, Set) ->
                                  Set1 = add_to_set(A,B,Set),
                                  Set2 = add_to_set(B,C,Set1),
                                  Set3 = add_to_set(C,A,Set2),
                                  Set3
                          end,
                          gb_sets:new(),
                          IndexTriplets),
    Edges = gb_sets:to_list(EdgeSet),
    {EdgesWithIndex, _} = lists:foldl(fun(Edge, {WithIndex, N}) ->
                                              {[{Edge, N}|WithIndex], N+1}
                                      end,
                                      {[], 1},
                                      Edges),
    Triangles = lists:foldl(fun({A,B,C}, Acc) ->
                                    Triangle = {index_for_edge(A,B,EdgesWithIndex),
                                                index_for_edge(B,C,EdgesWithIndex),
                                                index_for_edge(C,A,EdgesWithIndex)},
                                    [Triangle|Acc]
                                  end,
                            [],
                            IndexTriplets),
    Header = [io_lib:format("~p ~p ~p GtsSurface GtsFace GtsEdge GtsVertex", [length(Vertices), length(Edges), length(Triangles)])],
    VertexLines = lists:map(fun({X,Y,Z}) ->
                                    io_lib:format("~p ~p ~p", [X,Y,Z])
                            end, Vertices),
    EdgeLines = lists:map(fun({A,B}) ->
                                  io_lib:format("~p ~p", [A,B])
                          end, Edges),
    TriangleLines = lists:map(fun({A,B,C}) ->
                                  io_lib:format("~p ~p ~p", [A,C,B])
                          end, Triangles),
    iolist_to_binary(string:join(Header ++ VertexLines ++ EdgeLines ++ TriangleLines ++ [""], "\n")).
    
json_to_stl({Props}) ->
    {_, {FaceProps}} = lists:keyfind(<<"faces">>, 1, Props),
    {_, Positions} = lists:keyfind(<<"positions">>, 1, FaceProps),
    Vertices = lists:reverse(
                 lists:foldl(fun(Value, []) ->
                                     [{Value}];
                                (Value, [{X}|Rest]) ->
                                     [{X, Value}|Rest];
                                (Value, [{X,Y}|Rest]) ->
                                     [{X,Y,Value}|Rest];
                                (Value, Acc = [{_X,_Y,_Z}|_Rest]) ->
                                     [{Value}|Acc]
                             end,
                             [],
                             Positions)),
    io:format("~p~n", [Vertices]),
    {_, NormalsArray} = lists:keyfind(<<"normals">>, 1, FaceProps),
    Normals = lists:reverse(
                lists:foldl(fun(Value, []) ->
                                    [{Value}];
                               (Value, [{X}|Rest]) ->
                                    [{X, Value}|Rest];
                               (Value, [{X,Y}|Rest]) ->
                                    [{X,Y,Value}|Rest];
                               (Value, Acc = [{_X,_Y,_Z}|_Rest]) ->
                                    [{Value}|Acc]
                            end,
                            [],
                            NormalsArray)),
    {_, Indices} =  lists:keyfind(<<"indices">>, 1, FaceProps),
    IndexTriplets = lists:foldl(fun(Value, []) ->
                                        [{Value}];
                                   (Value, [{A}|Rest]) ->
                                        [{A, Value}|Rest];
                                   (C, [{A,B}|Rest]) ->
                                        [{A,B,C}|Rest];
                                   (Value, Acc = [{_A,_B,_C}|_Rest]) ->
                                        [{Value}|Acc]
                                end,
                                [],
                                Indices),
    io:format("~p~n", [IndexTriplets]),
    Triangles = lists:foldl(fun({A,B,C}, Acc) ->
                                    {V1X, V1Y, V1Z} = find_vertex(Vertices, A),
                                    {V2X, V2Y, V2Z} = find_vertex(Vertices, B),
                                    {V3X, V3Y, V3Z} = find_vertex(Vertices, C),
                                    {N1X, N1Y, N1Z} = find_vertex(Normals, A),
                                    {N2X, N2Y, N2Z} = find_vertex(Normals, B),
                                    {N3X, N3Y, N3Z} = find_vertex(Normals, C),
                                    [{{(N1X+N2X+N3X)/3, (N1Y+N2Y+N3Y)/3, (N1Z+N2Z+N3Z)/3},
                                      {V1X, V1Y, V1Z}, {V2X, V2Y, V2Z}, {V3X, V3Y, V3Z}} |Acc]
                                  end,
                            [],
                            IndexTriplets),
    iolist_to_binary("solid shapesmith_solid\n" ++ lists:map(fun to_facet/1, Triangles) ++ "endsolid shapesmith_solid\n").


to_facet({{Nx, Ny, Nz}, {V1x, V1y, V1z}, {V2x, V2y, V2z}, {V3x, V3y, V3z}}) ->
    io_lib:format("facet normal ~e ~e ~e\nouter loop\nvertex ~e ~e ~e\nvertex ~e ~e ~e\nvertex ~e ~e ~e\nendloop\nendfacet\n",
                  [Nx, Ny, Nz, V1x, V1y, V1z, V2x, V2y, V2z, V3x, V3y, V3z]).

find_vertex(Vertices, Index) ->
    find_vertex(Vertices, Index, 0).

find_vertex([Hd|_Rest], Index, Index) -> 
    Hd;
find_vertex([_Hd|Rest], Index, N) -> 
    find_vertex(Rest, Index, N+1).

add_to_set(A,B, Set) when A > B ->
    add_to_set(B, A, Set);
add_to_set(A, B, Set) ->
    gb_sets:add({A,B}, Set).

index_for_edge(A,B, EdgesWithIndex) when A > B ->
    index_for_edge(B, A, EdgesWithIndex);
index_for_edge(A, B, EdgesWithIndex) ->
    case lists:keyfind({A,B}, 1, EdgesWithIndex) of
        {_, Index}  -> Index;
        false -> throw({not_found, {A,B}})
    end.

