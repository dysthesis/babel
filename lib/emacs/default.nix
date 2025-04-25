lib: {
  # Yoink github.com/talyz/fromElisp
  fromElisp = import (import ./npins).fromElisp {};
  parseUsePackages = import ./parseUsePackages.nix {inherit lib;};
  buildPackages = import ./buildPackages lib;
  orgTangle = import ./orgTangle.nix;
}
