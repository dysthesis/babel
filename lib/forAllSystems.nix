nixpkgs: {
  pkgs ? nixpkgs,
  overlays ? [],
  systems ? [],
}: f:
pkgs.lib.genAttrs systems (system: f (import pkgs {inherit system overlays;}))
