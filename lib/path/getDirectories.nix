lib:
let
  inherit (builtins)
	  attrNames
    readDir
    ;
  inherit (lib)
    filterAttrs
    ;
in
dir: 
  dir 
	|> readDir 
	|> filterAttrs (_name: value: value == "directory") 
	|> attrNames
