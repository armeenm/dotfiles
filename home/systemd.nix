{ config, pkgs, lib, root, user, ... }:

let
  rclone = service: {
    Unit = {
      Description = "Remote FUSE filesystem for ${service}";
      Documentation = "man:rclone(1)";
      After = "network-online.target";
      Wants = "network-online.target";
      PartOf = [ "default.target" ];
    };

    Service = {
      Environment = [ "PATH=/run/wrappers/bin:$PATH" ];
      ExecStart = ''
              ${pkgs.rclone}/bin/rclone mount \
                --config=%h/.config/rclone/rclone.conf \
                --vfs-cache-mode writes \
                --vfs-cache-max-size 100M \
                --log-level INFO \
                ${service}: %h/mnt/${service}
            '';

      # TODO: Use a config-derived path for this.
      ExecStop = "/run/wrappers/bin/fusermount -u %h/mnt/${service}";
    };
  };

in {
  systemd = {
    user = {
      startServices = "sd-switch";

      services = {
        easyeffects = {
          Unit = {
            Description = "Audio effects for PipeWire applications";
            Documentation = "https://github.com/wwmm/easyeffects/wiki";
            PartOf = [ "graphical-session.target" ];
          };

          Service = {
            ExecStart = "${pkgs.easyeffects}/bin/easyeffects --gapplication-service";
          };
        };

        rclone-cobalt = rclone "cobalt";
        rclone-oxygen = rclone "oxygen";
      };
    };
  };
}
