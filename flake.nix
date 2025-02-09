{
  description = "A collection of useful Nix expressions.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    treefmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    treefmt,
    ...
  }: let
    lib = import ./lib inputs;
    inherit (builtins) mapAttrs;
    # Systems to support
    systems = [
      "aarch64-linux"
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
    forAllSystems = lib.poincare.forAllSystems systems;

    treefmt' = forAllSystems (pkgs: treefmt.lib.evalModule pkgs ./formatters);
  in
    # Budget flake-parts
    mapAttrs (_: forAllSystems) {
      devShells = pkgs: {default = import ./shell pkgs;};
      # for `nix fmt`
      formatter = pkgs: treefmt'.${pkgs.system}.config.build.wrapper;
      # for `nix flake check`
      checks = pkgs: {
        formatting = treefmt'.${pkgs.system}.config.build.check self;
      };
    };
}
