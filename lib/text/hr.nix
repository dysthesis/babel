# Replace dots ('.') with dashes ('-') in `test`
text:
let
  inherit (builtins)
    split
    foldl'
    isList
    tail
    ;
  parts = split "." text;
in
foldl' (text: part: if isList part then "${text}-" else text) "" (tail parts)
