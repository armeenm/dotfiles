{ lib
, pkgs
, isHeadless
, ...
}:

let
  inherit (pkgs.stdenv) hostPlatform;
in {
  services = lib.optionalAttrs hostPlatform.isLinux {
    batsignal.enable = !isHeadless; # TODO: isPortable?
    mpd-mpris.enable = !isHeadless;
    mpris-proxy.enable = !isHeadless;
    playerctld.enable = !isHeadless;
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

    hypridle = {
      enable = !isHeadless;
      settings = {
        general = {
          after_sleep_cmd = "hyprctl dispatch dpms on";
          ignore_dbus_inhibit = false;
          lock_cmd = "pgrep hyprlock || hyprlock";
        };

        listener = [
          {
            timeout = 900;
            on-timeout = "hyprlock";
          }
          {
            timeout = 1200;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
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

    wlsunset = {
        enable = !isHeadless;
        latitude = 47.76;
        longitude = -122;
    };
  };
}
