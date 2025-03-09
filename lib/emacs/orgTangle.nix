pkgs: emacs: orgFile: name:
pkgs.stdenv.mkDerivation {
  inherit name;
  buildInputs = [emacs];
  src = orgFile;
  buildPhase = ''
    cp $src output.org
    emacs --batch -Q \
      --eval "(require 'org)" \
      --eval "(require 'ob-tangle)" \
      --eval "(org-babel-tangle \"output.org\")"
  '';
  installPhase = ''
    mkdir -p $out
    cp *.el $out/
  '';
}
