{ config, osConfig, pkgs, lib, root, user, ... }:

let
  inherit (osConfig.nixpkgs) hostPlatform;
  conf = "${root}/conf";
  home = config.home.homeDirectory;
in
{
  xdg = {
    enable = true;

    userDirs = {
      enable = hostPlatform.isLinux;
      desktop = "${home}/desktop";
      documents = "${home}/docs";
      download = "${home}/dl";
      music = "${home}/music";
      pictures = "${home}/media";
      publicShare = "${home}/shared";
      templates = "${home}/templates";
      videos = "${home}/media";
    };

    systemDirs.data = lib.optionals (hostPlatform.isLinux) [
      "/var/lib/flatpak/exports/share"
      "${home}/.local/share/flatpak/exports/share"
    ];

    configFile = {
      "zsh/.p10k.zsh".source = "${conf}/zsh/p10k.zsh";
      "yt-dlp/config".source = "${conf}/yt-dlp/config";
      "waybar/style.css".source = "${conf}/waybar/style.css";
    };
  };
}
