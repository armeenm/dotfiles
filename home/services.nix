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
  services = lib.optionalAttrs hostPlatform.isLinux {
    hyprpolkitagent.enable = !isHeadless;
    mpd-mpris.enable = !isHeadless;
    mpris-proxy.enable = !isHeadless;
    playerctld.enable = !isHeadless;
    poweralertd.enable = !isHeadless;
    pueue.enable = true;
    remmina.enable = !isHeadless;
    #safeeyes.enable = !isHeadless;
    wob.enable = !isHeadless;

    clipcat = {
      enable = !isHeadless;
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

    fusuma = {
      enable = !isHeadless;
      settings = {};
    };

    gromit-mpx = {
      #enable = !isHeadless;
    };

    hypridle = {
      enable = !isHeadless;
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
            timeout = 60;
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
      enable = true;
      transitions = {
        sunrise = {
          calendar = "*-*-* 06:00:00";
          requests = [
            [ "temperature" "6500" ]
          ];
        };
        daytime = {
          calendar = "*-*-* 09:00:00";
          requests = [
            [ "identity" ]
          ];
        };
        sunset = {
          calendar = "*-*-* 19:00:00";
          requests = [
            [ "temperature" "3500" ]
          ];
        };
      };
    };

    mako = {
      enable = !isHeadless;
      settings = {
        "mode=do-not-disturb".invisible = 1;
      };
    };

    mpd = {
      enable = !isHeadless;
      network.startWhenNeeded = true;
      extraConfig = ''
      audio_output {
        type "pipewire"
        name "Pipewire Playback"
      }
    '';
    };

    recoll = {
      enable = true;
      settings = {
        nocjk = true;

        topdirs = with config.xdg.userDirs; [
          "~/src"
          desktop
          documents
          download
        ];

        "~/src" = {
          "skippedNames+" = [
            "build"
            "node_modules"
            "result"
            "target"
          ];
        };
      };
    };
  };
}
