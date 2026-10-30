{ config
, lib
, pkgs
, isHeadless
, isPortable
, ...
}:

let
  inherit (pkgs.stdenv) hostPlatform;
in {
  services = {
    batsignal.enable = hostPlatform.isLinux && !isHeadless;
    hyprpolkitagent.enable = hostPlatform.isLinux && !isHeadless;
    mpris-proxy.enable = hostPlatform.isLinux && !isHeadless;
    playerctld.enable = hostPlatform.isLinux && !isHeadless;
    pueue.enable = true;
    safeeyes.enable = hostPlatform.isLinux && !isHeadless;
    wob.enable = hostPlatform.isLinux && !isHeadless;

    clipcat = {
      enable = hostPlatform.isLinux && !isHeadless;
      enableZshIntegration = true;
      menuSettings.finder = "builtin";
    };

    emacs = {
      enable = true;
      # TODO: Fix upstream.
      defaultEditor = false;

      client = {
        enable = true;
        arguments = [ "-n" "-t" "-c" ];
      };
    };

    gromit-mpx = {
      #enable = !isHeadless;
    };

    hypridle = {
      enable = !isHeadless && hostPlatform.isLinux;
      settings = {
        general = {
          before_sleep_cmd = "loginctl lock-session";
          after_sleep_cmd = "hyprctl dispatch dpms on";
          lock_cmd = "pidof hyprlock || hyprlock";
        };

        listener = [
          {
            timeout = 30;
            on-timeout = "pidof hyprlock && hyprctl dispatch dpms off";
            on-resume = "pidof hyprlock && hyprctl dispatch dpms on";
          }
          (lib.optionalAttrs isPortable {
            timeout = 150;
            on-timeout = "brightnessctl -s set 10";
            on-resume = "brightnessctl -r";
          })
          {
            timeout = 300;
            on-timeout = "loginctl lock-session";
          }
          {
            timeout = 330;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }
          (lib.optionalAttrs isPortable {
            timeout = 900;
            on-timeout = "systemctl suspend";
          })
        ];
      };
    };

    hyprsunset = {
      enable = !isHeadless && hostPlatform.isLinux;
      settings = {
        profile = [
          {
            time = "6:00";
            temperature = 6500;
          }
          {
            time = "9:00";
            identity = true;
          }
          {
            time = "19:00";
            temperature = 3500;
          }
        ];
      };
    };

    jankyborders = {
      enable = !isHeadless && hostPlatform.isDarwin;
    };

    mako = {
      enable = !isHeadless && hostPlatform.isLinux;
      settings = {
        "mode=do-not-disturb".invisible = 1;
      };
    };

    recoll = {
      # enable = hostPlatform.isLinux;
      settings = {
        nocjk = true;
        topdirs = [ "~/docs" ];
      };
    };
  };

  # polkitd drops every registered authentication agent when it restarts, and
  # NixOS restarts polkit on every switch. hyprpolkitagent only registers once
  # at startup, so watch the system bus and restart it when polkitd comes back,
  # or polkit prompts silently never appear (e.g. KDE Partition Manager).
  systemd.user = lib.mkIf (hostPlatform.isLinux && !isHeadless) {
    services.polkit-agent-watch = {
      Unit = {
        Description = "Restart hyprpolkitagent when polkitd restarts";
        PartOf = [ config.wayland.systemd.target ];
        After = [ config.wayland.systemd.target "hyprpolkitagent.service" ];
      };

      Install.WantedBy = [ config.wayland.systemd.target ];

      Service = {
        Restart = "always";
        RestartSec = "5s";
        ExecStart = toString (pkgs.writeShellScript "polkit-agent-watch" ''
          set -u

          # gdbus uses a normal signal subscription; dbus-broker refuses
          # unprivileged monitoring, so dbus-monitor never sees these signals.
          # stdbuf and --line-buffered are both needed or nothing arrives
          # until the pipeline exits.
          # Matches only the acquire signal: the new owner (third argument)
          # must be non-empty, which excludes polkitd going away.
          ${pkgs.coreutils}/bin/stdbuf -oL ${lib.getBin pkgs.glib}/bin/gdbus monitor --system \
            --dest org.freedesktop.DBus --object-path /org/freedesktop/DBus \
          | ${pkgs.gnugrep}/bin/grep --line-buffered -E \
            "NameOwnerChanged \('org\.freedesktop\.PolicyKit1', '[^']*', '[^']+'\)$" \
          | while read -r _; do
              ${pkgs.systemd}/bin/systemctl --user restart hyprpolkitagent.service || true
            done
        '');
      };
    };
  };
}
