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
    modulesPath,
    ...
  }:
    nixosSystem {
      inherit system;
      modules = let
        profilesPath = "${modulesPath}/profiles";
      in
        [
          {networking.hostName = hostname;}
          {nixpkgs.hostPlatform = mkDefault system;}
          {
            imports = [
              "${profilesPath}/minimal.nix"
              "${profilesPath}/hardened.nix"
            ];
          }
        ]
        ++ config;
      specialArgs = {inherit self inputs;} // specialArgs;
    }
