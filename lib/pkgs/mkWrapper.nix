let
  inherit (builtins)
    isList
    elemAt
    ;
in
pkgs: package: postBuild:
let
  name = if isList package then elemAt package 0 else package;
  paths = if isList package then package else [ package ];
in
pkgs.symlinkJoin {
  inherit paths postBuild;
  name = "${name}-wrapped";
  buildInputs = [ pkgs.makeWrapper ];
}
