{ config, sys, pkgs, lib, root, user, ... }:

{
  user = {
    startServices = "sd-switch";

    services = {
      easyeffects = {
        Unit = {
          Description = " Audio effects for PipeWire applications";
          Documentation = "https://github.com/wwmm/easyeffects/wiki";
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          ExecStart = "${pkgs.easyeffects}/bin/easyeffects --gapplication-service";
        };
      };

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
              config.services.mako.package
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
