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
dir: type: 
  dir 
	|> readDir 
	|> filterAttrs (_name: value: value == type) 
	|> attrNames
