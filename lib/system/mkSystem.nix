lib: let
  inherit
    (lib)
    nixosSystem
    mkDefault
    ;
in
  {
    self,
    system,
    hostname,
    config,
    inputs,
    specialArgs,
    ...
  }:
    nixosSystem {
      inherit system;
      modules =
        [
          {networking.hostName = hostname;}
          {nixpkgs.hostPlatform = mkDefault system;}

          ({modulesPath, ...}: let
            profilesPath = "${modulesPath}/profiles";
          in {
            imports = [
              "${profilesPath}/minimal.nix"
              "${profilesPath}/hardened.nix"
            ];
          })
        ]
        ++ config;
      specialArgs = {inherit self inputs;} // specialArgs;
    }
