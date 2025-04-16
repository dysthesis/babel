pkgs: _final: _prev: let
  inherit (pkgs) callPackage;
in {
  fast-fonts = callPackage ./fast-fonts {};
  sf-pro = callPackage ./sf-pro {};
  generate-domains-blocklist = callPackage ./generate-domains-blocklist {};
  georgia-fonts = callPackage ./georgia-fonts {};
  cartograph-nf = callPackage ./cartograph-nf {};
  jbcustom-nf = callPackage ./jbcustom-nf {};
  oledppuccin-tmux = callPackage ./oledppuccin-tmux {};
  ropr = callPackage ./ropr {};
  # Broken for darwin
  ghostty-hardened = callPackage ./ghostty-hardened {};
  askii = callPackage ./askii {};
  fzf-bibtex = callPackage ./fzf-bibtex {};
}
