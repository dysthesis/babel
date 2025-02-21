lib:
let
  inherit (builtins)
    readDir
    ;
  inherit (lib)
    filterAttrs
    ;
in
dir: dir |> readDir |> filterAttrs (_name: value: value == "directory") |> attrNames
