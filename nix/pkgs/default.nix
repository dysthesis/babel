{
  perSystem =
    {
      craneLib,
      pkgs,
      commonArgs,
      cargoArtifacts,
      ...
    }:
    let
      inherit (pkgs) callPackage;
    in
    {
      packages = rec {
        package = callPackage ./package {
          inherit
            craneLib
            pkgs
            commonArgs
            cargoArtifacts
            ;
        };
        default = package;
      };
    };
}
