# Wraps a binary with the given postBuild argument
#
# Taken from:
# https://github.com/hlissner/dotfiles/blob/00d358f060f79c9f406d20dd659e109b44948370/lib/pkgs.nix#L17-L24
#
# Usage:
# mkWrapper program ''
#   wrapProgram "$out/bin/program" --add-flags "${flags}" \
#                                  --run "${run}"
# ''
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
