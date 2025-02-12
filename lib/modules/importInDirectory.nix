lib: let
  inherit (lib.babel.path) getDirectories;
in
  dir: dir
	     |> getDirectories
			 |> map (module: "${dir}/${module}")
