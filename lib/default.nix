nixpkgs:
nixpkgs.lib.extend (_final: _prev: {
  poincare = {
    forAllSystems = import ./forAllSystems.nix nixpkgs;
    mkNeovim = import ./mkNeovim.nix;
    plugin = import ./mkPlugin.nix _final;
  };
})
