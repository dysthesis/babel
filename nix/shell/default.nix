{
  perSystem = {
    pkgs,
    craneLib,
    ...
  }: let
    valeConfigured = pkgs.callPackage ./vale {};
  in {
    devShells.default = craneLib.devShell {
      packages = with pkgs; [
        # Nix
        nixd
        statix
        deadnix
        alejandra

        # Rust
        cargo-audit
        cargo-expand
        cargo-nextest
        cargo-llvm-cov
        cargo-llvm-lines
        cargo-show-asm
        rust-analyzer
        cargo-wizard
        bacon
        libllvm
        gnuplot

        # Prose
        valeConfigured

        # Miscellaneous
        jq
      ];
    };
  };
}
