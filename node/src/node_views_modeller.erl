-module(node_views_modeller).
-compile(export_all).

session() ->
    false.

designs() ->
    [dict:from_list([{design, Name}]) || Name <- ["iphonedock", "aerofoil"]].
