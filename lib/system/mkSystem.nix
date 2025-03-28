lib:
let
  inherit (lib)
    nixosSystem
    mkDefault
    checkListOfEnum
    removeSuffix
    ;
  inherit (lib.babel.path) getFiles;
in
{
  self,
  system,
  hostname,
  config,
  inputs,
  specialArgs,
  profiles ? [ ],
  ...
}:
nixosSystem {
  inherit system;
  modules = [
    { networking.hostName = hostname; }
    { nixpkgs.hostPlatform = mkDefault system; }

    (
      { modulesPath, ... }:
      let
        profilesPath = "${modulesPath}/profiles";
        validProfiles = map (removeSuffix ".nix") (getFiles profilesPath);
      in
      (checkListOfEnum "valid modules" validProfiles profiles) {
        imports = map (profile: "${profilesPath}/${profile}.nix") profiles;
      }
    )
    config
  ];
  specialArgs = {
    inherit self inputs;
  } // specialArgs;
}
