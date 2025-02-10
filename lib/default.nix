nixpkgs:
nixpkgs.lib.extend (_final: _prev: {
  babel = {
    forAllSystems = import ./forAllSystems.nix nixpkgs;
    nvim = import ./nvim _final;
    tmux = import ./tmux _final;
    text = import ./text;
  };
})
