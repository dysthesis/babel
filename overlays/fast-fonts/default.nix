{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  ...
}:
stdenvNoCC.mkDerivation rec {
  pname = "fast-font";
  version = src.rev;

  src = fetchFromGitHub {
    owner = "Born2Root";
    repo = "Fast-Font";
    rev = "f1ec8f9426c5907ac0e1f0d30464c5ac07c1f844";
    hash = "sha256-sbm3zOdvdrVA4j/oU1InQ04hroQw5XHlN9p233FU0TA=";
  };

  installPhase = ''
    runHook preInstall

    install -Dm644 *.ttf -t $out/share/fonts/truetype

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://github.com/Born2Root/Fast-Font";
    description = "A font to help you read faster.";
    license = licenses.mit;
    maintainers = [];
  };
}
