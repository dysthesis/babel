lib:
let
  inherit (lib.babel.path) getItemsOfType;
in
dir: getItemsOfType dir "regular"
