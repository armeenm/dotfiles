{ config, pkgs, lib, root, user, ... }:

let
  conf = "${root}/conf";
  home = "/home/${user.login}";
in
{
  enable = true;

  userDirs = {
    enable = true;
    desktop = "${home}/desktop";
    documents = "${home}/docs";
    download = "${home}/dl";
    music = "${home}/music";
    pictures = "${home}/media";
    publicShare = "${home}/shared";
    templates = "${home}/templates";
    videos = "${home}/media";
  };

  systemDirs.data = [
    "/var/lib/flatpak/exports/share"
    "${home}/.local/share/flatpak/exports/share"
  ];

  configFile = {
    "zsh/.p10k.zsh".source = "${conf}/zsh/p10k.zsh";
    "yt-dlp/config".source = "${conf}/yt-dlp/config";
    "waybar/style.css".source = "${conf}/waybar/style.css";
  };
}
