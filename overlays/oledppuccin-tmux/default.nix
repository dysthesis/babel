{
  tmuxPlugins,
  fetchFromGitHub,
}:
tmuxPlugins.mkTmuxPlugin rec {
  pluginName = "oledppuccin-tmux";
  version = "48582ad345c24dda3202f78103969f10ae3da080";
  rtpFilePath = "oledppuccin.tmux";
  src = fetchFromGitHub {
    owner = "dysthesis";
    repo = "oledppuccin-tmux";
    rev = version;
    sha256 = "vcVi+sm6DTTrcnutfphc16pPYinuXnRb1+QFMZEFG5M=";
  };
}
