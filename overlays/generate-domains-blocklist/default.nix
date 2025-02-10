{
  pkgs,
  lib,
  ...
}: let
  inherit (pkgs.stdenvNoCC) mkDerivation;
  inherit (pkgs) callPackage fetchFromGitHub;
  sources = callPackage ./_sources/generated.nix {inherit fetchFromGitHub;};
in
  mkDerivation {
    inherit (sources.dnscrypt-proxy-utils) src;
    name = "generate-domains-blocklist";

    installPhase =
      /*
      sh
      */
      ''
        mkdir -p $out
        cp $src/utils/generate-domains-blocklist/* $out/
      '';

    meta = with lib; {
      description = "Utilities to generate blocklist for DNSCrypt-Proxy";
      platforms = platforms.all;
    };
  }
