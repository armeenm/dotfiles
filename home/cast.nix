{ lib, pkgs, ... }:

let
  # One-command casting of this machine's desktop to the TV, which is a Mac
  # mini (rhenium) running Moonlight fullscreen.
  #
  # Two details make it scriptable, both found the hard way:
  #  * Moonlight-Qt 6.2 has a CLI (`stream`, `quit`, `list`, `pair`), so no
  #    GUI clicking is needed.
  #  * It must be started through LaunchServices (`open -b ...`), not from the
  #    ssh session. A `moonlight stream` run over ssh dies with
  #    "Quit event received" the moment that connection closes.
  cast = pkgs.writeShellApplication {
    name = "cast";
    # systemd only exists as a package on Linux; this module is imported by the
    # darwin hosts too, where referencing it breaks flake evaluation outright.
    runtimeInputs = [ pkgs.openssh ]
      ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux [ pkgs.systemd ];
    text = ''
      # Override any of these in the environment.
      RHENIUM="''${RHENIUM:-rhenium.local}"
      HOST="''${CAST_HOST:-argentum.local}"
      APP="''${CAST_APP:-Desktop}"
      BITRATE="''${CAST_BITRATE:-30000}"
      RES="''${CAST_RES:-1080}"
      FPS="''${CAST_FPS:-60}"

      remote() { ssh -o BatchMode=yes "$RHENIUM" "$@"; }

      case "''${1:-start}" in
        start)
          # The host side has to be up before the client can connect. On a
          # non-Linux host there is no local Sunshine to start.
          if command -v systemctl >/dev/null 2>&1; then
            systemctl --user is-active --quiet sunshine \
              || systemctl --user start sunshine
          fi
          remote "
            moonlight quit >/dev/null 2>&1 || true
            sleep 1
            open -b com.moonlight-stream.Moonlight --args stream '$HOST' '$APP' \
              --$RES --fps $FPS --bitrate $BITRATE --display-mode fullscreen
            # LaunchServices starts the stream without taking focus, so a
            # terminal keeps sitting in front of it. Re-activating the bundle
            # once the window has mapped raises it (and its fullscreen space).
            sleep 3
            open -b com.moonlight-stream.Moonlight
          "
          echo "casting $APP from $HOST to $RHENIUM"
          ;;
        stop)
          remote "moonlight quit >/dev/null 2>&1 || pkill -f 'MacOS/Moonlight' || true"
          # Sunshine must not keep listening once the stream is over; it is
          # only running because `cast start` brought it up.
          if command -v systemctl >/dev/null 2>&1; then
            systemctl --user stop sunshine || true
          fi
          echo "stream stopped"
          ;;
        status)
          remote "pgrep -fl 'MacOS/Moonlight .*stream' || echo 'not streaming'"
          ;;
        list)
          remote "moonlight list '$HOST'"
          ;;
        *)
          echo "usage: cast {start|stop|status|list}" >&2
          exit 2
          ;;
      esac
    '';
  };

  grab = pkgs.writeShellApplication {
    name = "grab";
    runtimeInputs = [ pkgs.moonlight-qt ];
    text = ''
      if [ "$#" -lt 1 ]; then
        echo "usage: grab <host> [app]" >&2
        exit 2
      fi
      HOST="$1"
      APP="''${2:-''${GRAB_APP:-Desktop}}"
      RES="''${GRAB_RES:-1080}"
      FPS="''${GRAB_FPS:-60}"
      BITRATE="''${GRAB_BITRATE:-30000}"
      MODE="''${GRAB_MODE:-windowed}"

      exec moonlight stream "$HOST" "$APP" \
        --"$RES" --fps "$FPS" --bitrate "$BITRATE" --display-mode "$MODE"
    '';
  };
in
{
  home.packages = [ cast grab ];
}
