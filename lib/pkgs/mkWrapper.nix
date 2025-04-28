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
  inherit (builtins) isList elemAt;
in
  pkgs: package: postBuild:
  # 1) Pick a “primary” package if the input is a list, otherwise use it directly.
  let
    target =
      if isList package
      then elemAt package 0
      else package;
    paths =
      if isList package
      then package
      else [package];
  in
    # 2) Create the wrapper, copying over mainProgram from the original meta
    pkgs.symlinkJoin {
      inherit paths postBuild;

      # name‐interpolation on a derivation uses its ‘name’ attribute
      name = "${target}-wrapped";

      buildInputs = [pkgs.makeWrapper];

      # --- NEW: inherit the original mainProgram ---
      # If target.meta.mainProgram is set, carry it through.
      meta = {
        inherit (target.meta) mainProgram;
      };
    }
