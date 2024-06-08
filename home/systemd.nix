{ config, pkgs, lib, root, user, ... }:

{
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

        rclone-cobalt = {
          Unit = {
            Description = "Remote FUSE filesystem for Cobalt";
            Documentation = "man:rclone(1)";
            After = "network-online.target";
            Wants = "network-online.target";
            PartOf = [ "default.target" ];
          };

          Service = {
            ExecStart = ''
              ${pkgs.rclone}/bin/rclone mount \
                --config=%h/.config/rclone/rclone.conf \
                --vfs-cache-mode writes \
                --vfs-cache-max-size 100M \
                --log-level INFO \
                cobalt: %h/mnt/cobalt
            '';

            # TODO: Use a config-derived path for this.
            ExecStop = "/run/wrappers/bin/fusermount -u %h/mnt/cobalt";
          };
        };
      };
    };
  };
}
