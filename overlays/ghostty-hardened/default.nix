{pkgs ? import <nixpkgs> {}}:
pkgs.ghostty.overrideAttrs
(old: {
  preBuild =
    (old.preBuild or "")
    +
    # bash
    ''
      shopt -s globstar
      sed -i 's/^const xev = @import("xev");$/const xev = @import("xev").Epoll;/' **/*.zig
      shopt -u globstar
    '';
})
