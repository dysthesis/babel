lib: {
  mkNeovim = import ./mkNeovim.nix;
  mkNvimPlugin = import ./mkNvimPlugin.nix;
  mapPlugins = import ./mapPlugins.nix lib;
}
