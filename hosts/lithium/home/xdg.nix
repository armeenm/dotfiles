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
    "profanity/profrc".source = "${conf}/profanity/profrc";
    "zsh/.p10k.zsh".source = "${conf}/zsh/p10k.zsh";

    "river/init" = {
      source = "${conf}/river/init";
      executable = true;
    };
  };
}
