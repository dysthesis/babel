pkgs: src: pname: revision:
pkgs.vimUtils.buildVimPlugin {
  inherit pname src;
  version =
    if (src ? lastModifiedDate)
    then src.lastModifiedDate
    else revision;
}
