nixpkgs:
{
  pkgs ? nixpkgs,
  overlays ? [ ],
  systems ? [ ],
}:
f:
pkgs.lib.genAttrs systems (
  system:
  let
    pkgs' = import pkgs { inherit system overlays; };
  in
  f pkgs'
)
