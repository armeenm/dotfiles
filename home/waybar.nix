{ lib, pkgs, isHeadless, isPortable, ... }:

let
  inherit (pkgs.stdenv) hostPlatform;
in {
  programs.waybar = {
    enable = hostPlatform.isLinux && !isHeadless;
    systemd.enable = true;

    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 40;

        output = [ "*" ];

        modules-left = [
          "hyprland/workspaces"
          "hyprland/submap"
          "custom/separator0"
          "group/mpris"
        ];

        modules-center = [ "hyprland/window" ];

        modules-right = [
          "cava"
          "custom/separator1"
          "group/pulse"
          "custom/separator0"
          "bluetooth"
          "custom/separator0"
          "network"
          "custom/separator0"
          "cpu"
          "temperature"
          "memory"
          "custom/separator0"
        ] ++ (lib.optionals isPortable [
          "backlight"
          "battery"
          "custom/separator0"
        ]) ++ [
          "systemd-failed-units"
          "idle_inhibitor"
          "custom/dnd"
          "clock"
          "group/tray"
        ];

        "group/pulse" = {
          orientation = "inherit";
          drawer = {
            transition-left-to-right = false;
          };
          modules = [
            "pulseaudio"
            "pulseaudio/slider"
          ];
        };

        "group/mpris" = {
          orientation = "inherit";
          drawer = {
            click-to-reveal = true;
            transition-left-to-right = false;
          };
          modules = [
            "custom/whitespace"
            "mpris"
          ];
        };

        "group/tray" = {
          orientation = "inherit";
          drawer = {};
          modules = [
            "custom/separator2"
            "tray"
            "custom/separator0"
            "custom/power"
            "custom/separator1"
          ];
        };

        "custom/separator0".format = " | ";
        "custom/separator1".format = " ";
        "custom/separator2".format = "  ";
        "custom/whitespace".format = lib.strings.replicate 10 " ";

        "custom/dnd" = {
          format = " {} ";
          interval = "once";
          signal = 1;
          exec = pkgs.writeShellScript "waybar-dnd" ''
              if [ $(makoctl mode) = "do-not-disturb" ]; then
                echo 󰂛
              else
                echo 󰂚
              fi
            '';
        };

        "custom/power" = {
          format = " ⏻ ";
          on-click = "wlogout";
        };

        bluetooth = {
          on-click = "hyprctl dispatch exec [float] foot bluetuith";
        };

        cava = {
          actions.on-click-right = "mode";
          autosens = 1;
          bar_delimiter = 0;
          bars = 14;
          format-icons = ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"];
          framerate = 60;
          sleep_timer = 5;
          hide_on_silence = true;
          waves = false;
          lower_cutoff_freq = 1;
          higher_cutoff_freq = 10000;
        };

        clock = {
          format = "{:%H:%M:%S %Z}";
          format-alt = "{:%A, %B %d, %Y (%R %Z)}";
          tooltip-format = "{tz_list}";
          interval = 1;
          on-click-middle = "hyprctl dispatch exec [float] foot clock-rs";
          timezones = [
            "America/Los_Angeles"
            "America/New_York"
            "Asia/Kolkata"
            "Etc/UTC"
          ];
        };

        cpu = {
          format = "CPU: {}%";
        };

        "hyprland/window" = {
          separate-outputs = true;
          rewrite."(\\[Sidebery\\] )?(.*) — Mozilla Firefox" = " $2";
        };

        memory = {
          format = "RAM: {}%";
        };

        mpris = {
          format = "{player_icon} {status_icon} {dynamic}";
          interval = 1;
          dynamic-len = 100;
          dynamic-order = [
            "position"
            "length"
            "artist"
            "title"
          ];
          player-icons = {
            default = "";
            firefox = "";
            Feishin = "";
          };
          status-icons = {
            playing = "";
            paused = "";
          };
        };

        pulseaudio = {
          format = "{volume}% {icon}";
          format-bluetooth = "{volume}% {icon}";
          format-muted = "MUTE ";
          format-icons = {
            default = ["" ""];
          };
          on-click = "hyprctl dispatch exec [float] ${pkgs.pavucontrol}/bin/pavucontrol";
          on-click-right = "${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
          scroll-step = 5.0;
          ignored-sinks = [ "Easy Effects Sink" ];
        };

        systemd-failed-units = {
          format = "{nr_failed_user}/{nr_failed_system} failed ";

          on-click = let
            script = pkgs.writeShellScript "waybar-systemd" ''
                echo "======== FAILED USER UNITS ========"
                systemctl --user --state=failed --no-pager
                echo "======== FAILED SYSTEM UNITS ========"
                systemctl --state=failed --no-pager
                read -p "Press enter to exit..."
              '';
          in "hyprctl dispatch exec [float] foot ${script}";
        };
      };
    };

    style = ''
        window#waybar {
          background: transparent;
        }
        #pulseaudio-slider slider {
          min-height: 0px;
          min-width: 0px;
          opacity: 0;
          background-image: none;
          border: none;
          box-shadow: none;
        }
        #pulseaudio-slider trough {
          min-height: 2px;
          min-width: 80px;
          border-radius: 0px;
          background-color: black;
        }
        #pulseaudio-slider highlight {
          min-width: 2px;
          border-radius: 0px;
          background-color: white;
        }
        * {
          font-family: "Cozette";
          font-size: 11pt;
        }
      '';
  };

}
