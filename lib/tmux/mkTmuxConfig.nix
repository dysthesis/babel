# Originally from
# https://github.com/jakehamilton/tmux/blob/7c353d3831231f4e358c3692a8cac76924381c05/lib/default.nix
lib: let
  inherit (lib.babel.text) hr;
in
  # Create a tmux configuration file.
  # Type: Attrs -> Path
  # Usage: mkConfig { inherit pkgs; shell = "${pkgs.bash}/bin/bash"; plugins = [ pkgs.tmuxPlugins.nord ]; extra-config = "set -g history-limit 1000"; }
  # Result: /nix/store/<hash>-tmux.conf
  {
    pkgs,
    terminal ? "screen-256color-bce",
    plugins ? [],
    extra-config ? "",
  }: let
    is-package = pkgs.lib.types.package.check;
    get-plugin-name = plugin:
      if is-package plugin
      then plugin.pname
      else plugin.plugin.pname;

    base-config = ''
      set -g default-terminal "${terminal}"
    '';

    plugin-config =
      pkgs.lib.concatMapStringsSep
      "\n\n"
      (plugin: ''
        # ${get-plugin-name plugin}
        # ${hr (get-plugin-name plugin)}
        ${plugin.extraConfig or ""}
        run-shell ${
          if is-package plugin
          then plugin.rtp
          else plugin.plugin.rtp
        }
      '')
      plugins;
  in
    pkgs.writeText "tmux.conf" ''
      #========================#
      #          BASE          #
      #========================#

      ${base-config}

      #========================#
      #         PLUGINS        #
      #========================#

      ${plugin-config}

      #========================#
      #      EXTRA CONFIG      #
      #========================#

      ${extra-config}
    ''
