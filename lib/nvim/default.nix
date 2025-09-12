lib: {
  mkNeovim = import ./mkNeovim.nix lib;
  mkNvimPlugin = import ./mkNvimPlugin.nix;
  mapPlugins = import ./mapPlugins.nix lib;
}
