lib: {
  mkNeovim = import ./mkNeovim.nix;
  plugin = import ./mkPlugin.nix lib;
}
