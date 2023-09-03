{ config, pkgs, lib, sys, root, user, ... }:

{
  mpris-proxy.enable = true;
  playerctld.enable = true;

  emacs = {
    enable = true;
    # TODO: Fix upstream.
    defaultEditor = false;

    client = {
      enable = true;
      arguments = [ "-n" "-t" "-c" ];
    };
  };

  mako = {
    enable = true;
    extraConfig = ''
      [mode=do-not-disturb]
      invisible=1
    '';
  };

  swayidle =
    let
      systemd = sys.systemd.package;
      hyprland = sys.programs.hyprland.package;
    in {
      enable = true;
      events = [
        { event = "before-sleep"; command = "${systemd}/bin/loginctl lock-session"; }
        { event = "lock"; command = "${pkgs.swaylock}/bin/swaylock -fF -c 000000"; }
      ];
      timeouts = [
        { timeout = 60; command = "${systemd}/bin/loginctl lock-session"; }
        {
          timeout = 120;
          command = "${hyprland}/bin/hyprctl dispatch dpms off";
          resumeCommand = "${hyprland}/bin/hyprctl dispatch dpms on";
        }
      ];
      extraArgs = [ "idlehint" "60" ];
    };

  gammastep = {
    enable = false;
    latitude = 40.1019564;
    longitude = -88.2293502;
  };

  mpd = {
    enable = true;
    network.startWhenNeeded = true;
  };
}
