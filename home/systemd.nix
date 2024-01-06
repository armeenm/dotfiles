{ config, pkgs, lib, root, user, ... }:

{
  systemd = {
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
      };
    };
  };
}
