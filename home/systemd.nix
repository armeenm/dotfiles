{ config, pkgs, lib, root, user, ... }:

let
  rclone = service: {
    Install.WantedBy = [ "graphical-session.target" ];

    Unit = {
      Description = "Remote FUSE filesystem for ${service}";
      Documentation = "man:rclone(1)";
      PartOf = [ "graphical-session.target" ];
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
          Install.WantedBy = [ "graphical-session.target" ];

          Unit = {
            Description = "Audio effects for PipeWire applications";
            Documentation = "https://github.com/wwmm/easyeffects/wiki";
            PartOf = [ "graphical-session.target" ];
          };

          Service = {
            ExecStart = "${pkgs.easyeffects}/bin/easyeffects --gapplication-service";
          };
        };

        mpvpaper = {
          Install.WantedBy = [ "graphical-session.target" ];

          Unit = {
            Description = "Video wallpaper program for wlroots compositors";
            Documentation = "https://github.com/GhostNaN/mpvpaper";
            PartOf = [ "graphical-session.target" ];
          };

          Service = {
            ExecStart = ''
              /bin/sh -c " \
                ${pkgs.mpvpaper}/bin/mpvpaper -o \"term-status-msg= input-ipc-server=/run/user/$(id -u)/mpvpaper.sock\" ALL /home/armeen/.config/wallpaper.mp4 \
              "
             '';
          };
        };

        rclone-cobalt = rclone "cobalt";
        rclone-oxygen = rclone "oxygen";
      };
    };
  };
}
