lib: let
  inherit
    (builtins)
    readDir
    attrNames
    map
    ;
  inherit
    (lib)
    filterAttrs
    ;
in dir: dir
			|> readDir
			|> filterAttrs (_name: value: value == "regular")
			|> attrNames
			|> (xs: map (x: "${dir}/${x}") xs)
