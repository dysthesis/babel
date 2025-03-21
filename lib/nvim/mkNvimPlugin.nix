pkgs: src: pname:
pkgs.vimUtils.buildVimPlugin {
  inherit pname src;
  version = src.lastModifiedDate;
}
