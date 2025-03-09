lib:
let
  inherit (builtins)
    readDir
    attrNames
    ;
  inherit (lib)
    filterAttrs
    ;
in
dir: _type: dir |> readDir |> filterAttrs (_name: value: value == type) |> attrNames
