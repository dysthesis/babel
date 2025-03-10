lib: {
  # Yoink github.com/talyz/fromElisp
  fromElisp = import (import ./npins).fromElisp {};
  parsePackagesFromUsePackage = import ./parsePackagesFromUsePackage lib;
}
