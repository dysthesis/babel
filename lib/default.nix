nixpkgs:
nixpkgs.lib.extend (_final: _prev: {
  poincare = {
    forAllSystems = import ./forAllSystems.nix nixpkgs;
    nvim = import ./nvim _final;
  };
})
