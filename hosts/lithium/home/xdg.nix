{ config, pkgs, lib, root, user, ... }:

let
  conf = "${root}/conf";
in
{
  enable = true;
  
  userDirs = {
    enable = true;
    desktop = "\$HOME/desktop";
    documents = "\$HOME/docs";
    download = "\$HOME/dl";
    music = "\$HOME/music";
    pictures = "\$HOME/media";
    publicShare = "\$HOME/shared";
    templates = "\$HOME/templates";
    videos = "\$HOME/media";
  };
  
  configFile = {
    "xmobar".source = "${conf}/xmobar";
    "flameshot".source = "${conf}/flameshot";
    "zsh/.p10k.zsh".source = "${conf}/zsh/p10k.zsh";
  };
}
