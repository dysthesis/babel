nixpkgs: {
  overlays ? [],
  systems ? [],
}: f:
nixpkgs.lib.genAttrs systems (system: f (import nixpkgs {inherit system overlays;}))
