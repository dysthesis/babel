lib: pkgs: emacs: let
  inherit (lib.babel.emacs) orgTangle;
	inherit (lib) fold;
	inherit (builtins) 
	readFile
	readDir
	attrNames
	;
in orgFile: let
  tangleResults = orgTangle pkgs emacs orgFile "tangle-output";
in 
  tangleResults
	|> attrNames
	|> fold (curr: acc: let
	  path = "${tangleResults}/${curr}";
	 in acc // {
		  "${path}" = readFile path;
		}) {}
