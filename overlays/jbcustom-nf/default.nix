{
  lib,
  stdenvNoCC,
}:
stdenvNoCC.mkDerivation {
  pname = "JBCustom Nerd Font";
  version = "0.0.1";

  src = ./fonts;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/fonts/truetype
    cp *.ttf *.otf $out/share/fonts/truetype

    runHook postInstall
  '';

  meta = with lib; {
    description = "Customised JetBrainsMono Nerd Font to enable some font features.";
    platforms = platforms.all;
  };
}
