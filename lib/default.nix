nixpkgs:
nixpkgs.lib.extend (
  _final: _prev: {
    babel = {
      forAllSystems = import ./forAllSystems.nix nixpkgs;
      filesInDir = import ./filesInDir.nix _final;
      nvim = import ./nvim _final;
      tmux = import ./tmux _final;
      text = import ./text;
      system = import ./system _final;
      path = import ./path _final;
      modules = import ./modules _final;
      pkgs = import ./pkgs;
      int = import ./int;
      emacs = import ./emacs _final;
    };
  }
)
