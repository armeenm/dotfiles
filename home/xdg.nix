{ config
, pkgs
, lib
, ...
}:

let
  inherit (pkgs.stdenv) hostPlatform;
  conf = ../conf;
  home = config.home.homeDirectory;
in
{
  xdg = {
    enable = true;
    autostart.enable = false;
    portal.config.common.default = "hyprland";

    configFile = {
      "zsh/p10k.zsh".source = ../conf/zsh/p10k.zsh;
      "satty/config.toml".source = ../conf/satty/config.toml;
    };

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

    systemDirs.data = lib.optionals hostPlatform.isLinux [
      "/var/lib/flatpak/exports/share"
      "${home}/.local/share/flatpak/exports/share"
    ];
  };
}
