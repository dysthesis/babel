{
  pkgs,
  src,
  pname,
  version ? src.lastModifiedDate,
}:
pkgs.vimUtils.buildVimPlugin {
  inherit pname src version;
}
