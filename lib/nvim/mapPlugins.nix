# Taken from https://github.com/jordanisaacs/neovim-flake/blob/main/modules/lib/plugins.nix
lib: let
  inherit
    (lib)
    mapAttrs'
    nameValuePair
    removePrefix
    hasPrefix
    filterAttrs
    ;

  inherit (lib.attrsets) attrNames;
  inherit (lib.babel.nvim) mkNvimPlugin;

  # Get the names of all flake inputs that start with the given prefix.
  fromInputs = {
    inputs,
    prefix,
  }:
    mapAttrs' (n: v: nameValuePair (removePrefix prefix n) {src = v;}) (
      filterAttrs (n: _: hasPrefix prefix n) inputs
    );
in
  pkgs: inputs: prefix:
    map (x:
      mkNvimPlugin {
        inherit pkgs;
        src = inputs."${prefix}${x}";
        pname = x;
      }) (
      attrNames (fromInputs {
        inherit inputs prefix;
      })
    )
