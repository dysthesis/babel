{ rustPlatform, lib, fetchFromGitHub, python3, libxcb, ... }:
rustPlatform.buildRustPackage rec {
  pname = "askii";
  version = "0.6.0";

  src = fetchFromGitHub {
    owner = "nytopop";
    repo = pname;
    rev = "master";
    sha256 = "sha256-bdWBp8aPBJ3Us0MscvvVPutJEHyXFCPP55Fg8ZKSqnc";
  };

  nativeBuildInputs = [ python3 ];
  buildInputs = [ libxcb ];

  cargoHash = "sha256-ho9xzwd3w8SRfbmIgL8YskkbgeqIqU8rJV9jvc1rfs4=";

  meta = with lib; {
    description = "TUI based ASCII diagram editor.";
    homepage = "https://github.com/nytopop/askii";
    license = licenses.asl20;
  };
}
