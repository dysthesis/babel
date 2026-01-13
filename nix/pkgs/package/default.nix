{
  craneLib,
  cargoArtifacts,
  commonArgs,
  ...
}:
craneLib.buildPackage (
  commonArgs
  // {
    inherit cargoArtifacts;
    pname = "babel";
    CARGO_PROFILE = "release";
  }
)
