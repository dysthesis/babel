lib: {
  mkNeovim = import ./mkNeovim.nix;
  mapPlugins = import ./mapPlugins.nix lib;
}
