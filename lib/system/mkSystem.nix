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
let
  validModules = getDirectories "${modulesPath/profiles}";
in
checkListOfEnum "valid modules" profiles validModules nixosSystem {
  inherit system;
  modules = [
    { networking.hostName = hostname; }
    { nixpkgs.hostPlatform = mkDefault system; }

    (
      { modulesPath, ... }:
      let
        profilesPath = "${modulesPath}/profiles";
      in
      {
        imports = map profiles (profile: "${profilesPath}/${profile}.nix");
      }
    )
    config
  ];
  specialArgs = {
    inherit self inputs;
  } // specialArgs;
}
