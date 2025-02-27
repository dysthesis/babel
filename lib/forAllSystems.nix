nixpkgs: {
  overlays ? [],
  systems,
}: f:
nixpkgs.lib.genAttrs systems (system: let
  pkgs = import nixpkgs {inherit system overlays;};
in
  f pkgs)
