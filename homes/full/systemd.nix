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
          Environment = [ "PATH=${lib.makeBinPath (with pkgs; [ bash coreutils gnugrep pciutils ])}" ];
          ExecStart = "${sys.programs.hyprland.package}/bin/Hyprland";
        };
      };
    };
  };
}
