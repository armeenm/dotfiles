{ config, pkgs, lib, root, user, ... }:

let
  conf = "${root}/conf";
in
{
  enable = true;

  userDirs = {
    enable = true;
    desktop = "~/desktop";
    documents = "~/docs";
    download = "~/dl";
    music = "~/music";
    pictures = "~/media";
    publicShare = "~/shared";
    templates = "~/templates";
    videos = "~/media";
  };

  systemDirs.data = [
    "/var/lib/flatpak/exports/share"
    "~/.local/share/flatpak/exports/share"
  ];

  configFile = {
    "profanity/profrc".source = "${conf}/profanity/profrc";
    "zsh/.p10k.zsh".source = "${conf}/zsh/p10k.zsh";
    "yt-dlp/config".source = "${conf}/yt-dlp/config";
    "xdg-desktop-portal-wlr/river".source = "${conf}/river/xdg-desktop-portal-wlr";

    "river/init" = {
      source = "${conf}/river/init";
      executable = true;
    };
  };
}
