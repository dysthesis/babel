{
  inputs,
  self,
  pkgs,
  ...
}: let
  inherit (pkgs) system;
  overlay = self.overlays.${system};
  pkgs' = import inputs.nixpkgs {
    inherit system;
    overlays = [overlay];
  };
in {
  inherit
    (pkgs')
    sf-pro
    georgia-fonts
    generate-domains-blocklist
    cartograph-nf
    jbcustom-nf
    oledppuccin-tmux
    ropr
    ghostty-hardened
    askii
    fast-fonts
    ;
}
