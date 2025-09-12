lib: {
  mkNeovim = import ./mkNeovim.nix {inherit lib;};
  mkNvimPlugin = import ./mkNvimPlugin.nix;
  mapPlugins = import ./mapPlugins.nix lib;
}
