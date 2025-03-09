pkgs: emacs: orgFile: name:
pkgs.stdenv.mkDerivation {
  inherit name;
  buildInputs = [emacs];
  src = orgFile;
  unpackPhase = ''
    cp $src output.org
  '';
  buildPhase = ''
    emacs --batch \
     -l org output.org \
     -f org-babel-tangle
  '';
  installPhase = ''
    rm output.org
    mkdir -p $out
    cp * $out/
  '';
}
