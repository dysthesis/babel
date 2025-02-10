{
  pkgs ? import <nixpkgs> {},
  fetchFromGitHub,
}:
pkgs.rustPlatform.buildRustPackage rec {
  pname = "ropr";
  version = "0.2.25";

  cargoHash = "sha256-2slb0wbDqOiBtg3FVeVxiv3g8uwlXV9ueeloeb5Jd6Q=";

  src = fetchFromGitHub {
    owner = "Ben-Lichtman";
    repo = "ropr";
    rev = version;
    sha256 = "LfQp7knYlwzxyfA7NolYu9RQQAR3eBir6ULEiUOhQ7s=";
  };
}
