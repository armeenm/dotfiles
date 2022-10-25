{ config, sys, pkgs, lib, root, user, ... }:

{
  user = {
    startServices = "sd-switch";

    services = {
      hyprland = {
        Unit = {
          Description = "A dynamic tiling Wayland compositor that doesn't sacrifice on its looks.";
          Documentation = "https://wiki.hyprland.org";
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          Environment = [
            "PATH=${lib.makeBinPath (with pkgs; [
              bash
              coreutils
              gnugrep
              pciutils

              bemenu
              grim
              light
              pamixer
              slurp
              swappy
              swaylock

              sys.systemd.package

              config.programs.emacs.finalPackage
              config.programs.foot.package
              config.programs.mako.package
              config.services.playerctld.package
            ])}"

            "BEMENU_BACKEND=wayland"
          ];

          ExecStart = "${sys.programs.hyprland.package}/bin/Hyprland";
        };
      };
    };
  };
}
