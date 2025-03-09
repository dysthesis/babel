pkgs: emacs: orgFile: name:
pkgs.stdenv.mkDerivation {
  inherit name;
  buildInputs = [emacs];
  src = orgFile;
  unpackPhase = ''
    cp $src output.org
  '';
  buildPhase = ''
    emacs --batch -Q \
      --eval "(require 'org)" \
      --eval "(require 'ob-tangle)" \
      --eval "(org-babel-tangle \"output.org\")"
  '';
  installPhase = ''
    rm output.org
    mkdir -p $out
    cp * $out/
  '';
}
