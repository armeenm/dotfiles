{ config
, lib
, pkgs
, isHeadless
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

    wlsunset = {
        enable = !isHeadless;
        latitude = 47.76;
        longitude = -122;
    };
  };
}
