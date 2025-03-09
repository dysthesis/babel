{
  description = "A collection of useful Nix expressions.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    treefmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      treefmt,
      ...
    }@inputs:
    let
      mkLib = import ./lib;
      lib = mkLib nixpkgs;

      inherit (builtins) mapAttrs;
      # Systems to support
      systems = [
        "aarch64-linux"
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = lib.babel.forAllSystems { inherit systems; };

      treefmt' = forAllSystems (pkgs: treefmt.lib.evalModule pkgs ./formatters);
    in
    # Budget flake-parts
    mapAttrs (_: forAllSystems) {
      devShells = pkgs: { default = import ./shell pkgs; };
      # for `nix fmt`
      formatter = pkgs: treefmt'.${pkgs.system}.config.build.wrapper;
      # for `nix flake check`
      checks = pkgs: {
        formatting = treefmt'.${pkgs.system}.config.build.check self;
      };

      overlays = pkgs: import ./overlays pkgs;
      packages = pkgs: import ./packages { inherit inputs self pkgs; };
    }
    // {
      inherit mkLib;
    };
}
