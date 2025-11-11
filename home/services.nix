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
    batsignal.enable = !isHeadless;
    hyprpolkitagent.enable = !isHeadless;
    mpris-proxy.enable = !isHeadless;
    playerctld.enable = !isHeadless;
    pueue.enable = true;
    safeeyes.enable = !isHeadless;
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
      enable = true;
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

    mako = {
      enable = !isHeadless;
      settings = {
        "mode=do-not-disturb".invisible = 1;
      };
    };
  };
}
