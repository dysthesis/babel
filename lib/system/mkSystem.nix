lib:
let
  inherit (lib)
    nixosSystem
    mkDefault
    checkListOfEnum
    ;
  inherit (lib.babel.path) getDirectories;
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
        validModules = getDirectories profilesPath;
      in
      checkListOfEnum "valid modules" profiles validModules {
        imports = map profiles (profile: "${profilesPath}/${profile}.nix");
      }
    )
    config
  ];
  specialArgs = {
    inherit self inputs;
  } // specialArgs;
}
