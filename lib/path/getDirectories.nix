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
dir: dir |> readDir |> filterAttrs (_name: value: value == "directory") |> attrNames
