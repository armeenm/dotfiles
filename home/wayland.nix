{ lib
, pkgs
, inputs
, osConfig
, isHeadless
, isStandalone
, isPortable
, ...
}:

let
  inherit (pkgs.stdenv) hostPlatform;

  hyprPkgs = inputs.hyprland.packages.${hostPlatform.system};
  hyprsplitLua =
    inputs.hyprsplit.packages.${hostPlatform.system}.hyprsplitlua;

  toLua = lib.generators.toLua { };
  lua = lib.generators.mkLuaInline;

  mkBind = key: dispatcher: {
    _args = [
      key
      (lua dispatcher)
    ];
  };

  mkBindOpts = key: dispatcher: opts: {
    _args = [
      key
      (lua dispatcher)
      opts
    ];
  };

  # keys: rest of chord after mod, e.g. "Q" or "SHIFT + C"
  modBind = keys: dispatcher: {
    _args = [
      (lua ''mod .. " + ${keys}"'')
      (lua dispatcher)
    ];
  };

  modBindOpts = keys: dispatcher: opts: {
    _args = [
      (lua ''mod .. " + ${keys}"'')
      (lua dispatcher)
      opts
    ];
  };

  dir = {
    H = "left";
    J = "down";
    K = "up";
    L = "right";
  };

in {
  wayland = lib.optionalAttrs hostPlatform.isLinux {
    windowManager.hyprland = {
      enable = !isHeadless;

      package = if isStandalone
                then hyprPkgs.hyprland
                else osConfig.programs.hyprland.package;

      portalPackage = if isStandalone
                      then hyprPkgs.xdg-desktop-portal-hyprland
                      else osConfig.programs.hyprland.portalPackage;

      configType = "lua";

      systemd = {
        enable = isStandalone;
        variables = [ "--all" ];
      };

      plugins = let
        system = pkgs.stdenv.hostPlatform.system;
      in [
        inputs.hypr-dynamic-cursors.packages.${system}.default
      ];

      extraLuaFiles = {
        "hyprsplit/init" = {
          autoLoad = false;
          content = builtins.readFile "${hyprsplitLua}/share/hyprsplit/init.lua";
        };

        "00-hyprsplit" = ''
          hs = require("hyprsplit")
          hs.config({ num_workspaces = 10 })
        '';
      };

      settings = {
        mod = {
          _var = "SUPER";
        };

        monitor = {
          output = "";
          mode = "preferred";
          position = "auto";
          scale = 1;
          bitdepth = 10;
        };

        config = {
          decoration = {
            rounding = 0;
            blur.enabled = !isPortable;
            shadow.enabled = !isPortable;
          };

          input = {
            follow_mouse = 1;
            repeat_delay = 200;
            repeat_rate = 60;
            scroll_factor = 1.0;
            sensitivity = 0;

            touchpad = {
              clickfinger_behavior = true;
              natural_scroll = true;
              drag_3fg = 2;
            };
          };

          general = {
            gaps_in = 10;
            gaps_out = {
              top = 10;
              right = 20;
              bottom = 20;
              left = 20;
            };
            border_size = 2;
            resize_on_border = true;
            layout = "dwindle";
          };

          dwindle = {
            preserve_split = true;
          };

          group = {
            groupbar = {
              font_size = 18;
              gradients = true;
            };
          };

          plugin = {
            # hyphens in plugin option names are normalized to underscores for Lua
            dynamic_cursors = {
              enabled = true;
              mode = "none";
              hyprcursor.enabled = true;
              shake.enabled = true;
            };
          };

          misc = {
            disable_hyprland_logo = true;
            disable_splash_rendering = true;
            enable_anr_dialog = false;
            font_family = "Tamsyn";
          };
        };

        animation = [
          {
            leaf = "global";
            enabled = true;
            speed = 2;
            bezier = "default";
          }
          {
            leaf = "specialWorkspace";
            enabled = false;
          }
        ];

        gesture = {
          fingers = 3;
          direction = "horizontal";
          action = "workspace";
        };

        window_rule = [
          {
            name = "idle-inhibit-fullscreen";
            match.fullscreen = 1;
            idle_inhibit = "fullscreen";
          }
          {
            name = "zoom-menu-stay-focused";
            match = {
              class = "zoom";
              initial_title = "menu_window";
            };
            stay_focused = true;
          }
        ];

        bind = let
          selectWindow = pkgs.writeShellScript "hyprland-selectwin" ''
            set -euo pipefail
            hyprctl clients -j \
            | jq --argjson active $(hyprctl monitors -j \
            | jq -c \
                '[.[].activeWorkspace.id]') '.[] | select((.hidden | not) and .workspace.id as $id | $active | contains([$id])) | "\(.at[0]),\(.at[1]) \(.size[0])x\(.size[1])"' -r \
            | slurp
          '';

          screenrec = pkgs.writeShellScript "hyprland-screenrec" ''
            set -euo pipefail

            mode=$1
            shift 1

            filepath=~/ss/wl-screenrec-$(date '+%Y%m%d-%H:%M:%S').mp4
            cmd="wl-screenrec -f $filepath $@"

            if [[ $mode = region ]]; then
              $cmd -g "$(slurp)"
            elif [[ $mode = window ]]; then
              $cmd -g "$(${selectWindow})"
            elif [[ $mode = screen ]]; then
              $cmd -g "$(slurp -o)"
            else
              echo Unknown capture mode "$mode".
              exit -1
            fi

            notify-send "Screen recording saved" "Recording saved to $filepath" -a wl-screenrec
          '';

          screenshot = x:
            "hyprshot -zm ${x} -r - | satty -f - -o ~/ss/satty-$(date '+%Y%m%d-%H:%M:%S').png";

          backlight = pkgs.writeShellScript "hyprland-backlight" ''
            set -euo pipefail
            brightnessctl s "$1"
            brightnessctl -m \
            | awk -F',' '{print substr($4, 1, length($4)-1)}' \
            > "$XDG_RUNTIME_DIR"/wob.sock
          '';

          volume = pkgs.writeShellScript "hyprland-volume" ''
            set -euo pipefail
            wpctl set-volume @DEFAULT_AUDIO_SINK@ "$@"
            wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{ print $2*100 }' \
            > "$XDG_RUNTIME_DIR"/wob.sock
          '';

        in [
          (modBindOpts "mouse:272" "hl.dsp.window.drag()" { mouse = true; })
          (modBindOpts "mouse:273" "hl.dsp.window.resize()" { mouse = true; })

          (mkBindOpts "XF86AudioRaiseVolume"
            ''hl.dsp.exec_cmd(${toLua "${volume} -l 1.5 5%+"})''
            { repeating = true; })
          (mkBindOpts "XF86AudioLowerVolume"
            ''hl.dsp.exec_cmd(${toLua "${volume} 5%-"})''
            { repeating = true; })
          (mkBindOpts "XF86MonBrightnessUp"
            ''hl.dsp.exec_cmd(${toLua "${backlight} +5%"})''
            { repeating = true; })
          (mkBindOpts "XF86MonBrightnessDown"
            ''hl.dsp.exec_cmd(${toLua "${backlight} 5%-"})''
            { repeating = true; })

          (modBind "ALT + H"
            "hl.dsp.window.resize({ x = -30, y = 0, relative = true })")
          (modBind "ALT + J"
            "hl.dsp.window.resize({ x = 0, y = 30, relative = true })")
          (modBind "ALT + K"
            "hl.dsp.window.resize({ x = 0, y = -30, relative = true })")
          (modBind "ALT + L"
            "hl.dsp.window.resize({ x = 30, y = 0, relative = true })")

          (modBind "Q" "hl.dsp.window.close()")
          (modBind "backspace" "hl.dsp.force_renderer_reload()")
          (modBind "space" "hl.dsp.window.float({ action = \"toggle\" })")
          (modBind "D" ''hl.dsp.exec_cmd("exec $(tofi-run)")'')
          (modBind "P" ''hl.dsp.exec_cmd("emacsclient -c -n")'')
          (modBind "F" "hl.dsp.window.fullscreen({ mode = \"maximized\" })")

          (modBind "SHIFT + return" ''hl.dsp.exec_cmd("footclient")'')
          (modBind "SHIFT + Q" "hl.dsp.window.close()")
          (modBind "SHIFT + equal" "hl.dsp.exit()")
          (modBind "SHIFT + space" "hl.dsp.window.pin()")
          (modBind "SHIFT + D" ''hl.dsp.exec_cmd("exec $(tofi-drun)")'')
          (modBind "SHIFT + P" "hl.dsp.window.pseudo()")
          (modBind "SHIFT + F" "hl.dsp.window.fullscreen({ mode = \"fullscreen\" })")

          (modBind "C" ''hl.dsp.exec_cmd(${toLua (screenshot "region")})'')
          (modBind "SHIFT + C" ''hl.dsp.exec_cmd(${toLua (screenshot "window")})'')
          (modBind "CTRL + C" ''hl.dsp.exec_cmd(${toLua (screenshot "output")})'')

          (modBind "V" ''hl.dsp.exec_cmd(${toLua "${screenrec} region"})'')
          (modBind "SHIFT + V" ''hl.dsp.exec_cmd(${toLua "${screenrec} window"})'')
          (modBind "CTRL + V" ''hl.dsp.exec_cmd(${toLua "${screenrec} screen"})'')
          (modBind "ALT + V" ''hl.dsp.exec_cmd("pkill -SIGINT wl-screenrec")'')

          (modBind "W" ''hl.dsp.focus({ monitor = "l" })'')
          (modBind "E" ''hl.dsp.focus({ monitor = "r" })'')
          (modBind "SHIFT + W" ''hl.dsp.window.move({ monitor = "l" })'')
          (modBind "SHIFT + E" ''hl.dsp.window.move({ monitor = "r" })'')
          (modBind "ALT + W" ''hl.dsp.workspace.move({ monitor = "l" })'')
          (modBind "ALT + E" ''hl.dsp.workspace.move({ monitor = "r" })'')

          (modBind "tab" ''hl.dsp.exec_cmd("hyprswitch gui --mod-key super --key tab --close mod-key-release --reverse-key=key=grave --sort-recent && hyprswitch dispatch")'')

          (modBind "SHIFT + Z" ''hl.dsp.layout("togglesplit")'')
          (modBind "Z" ''hl.dsp.layout("swapsplit")'')
          (modBind "T" "hl.dsp.group.toggle()")

          (modBind "N" "hl.dsp.group.prev()")
          (modBind "M" "hl.dsp.group.next()")

          (modBind "H" ''hl.dsp.focus({ direction = "${dir.H}" })'')
          (modBind "J" ''hl.dsp.focus({ direction = "${dir.J}" })'')
          (modBind "K" ''hl.dsp.focus({ direction = "${dir.K}" })'')
          (modBind "L" ''hl.dsp.focus({ direction = "${dir.L}" })'')

          (modBind "SHIFT + H" ''hl.dsp.window.move({ direction = "${dir.H}" })'')
          (modBind "SHIFT + J" ''hl.dsp.window.move({ direction = "${dir.J}" })'')
          (modBind "SHIFT + K" ''hl.dsp.window.move({ direction = "${dir.K}" })'')
          (modBind "SHIFT + L" ''hl.dsp.window.move({ direction = "${dir.L}" })'')

          (modBind "minus" "hl.dsp.workspace.toggle_special(\"\")")
          (modBind "SHIFT + minus"
            ''hl.dsp.window.move({ workspace = "special", follow = false })'')

          (modBind "CTRL + J" ''hs.dsp.focus({ workspace = "e-1" })'')
          (modBind "CTRL + K" ''hs.dsp.focus({ workspace = "e+1" })'')

          (modBind "mouse_up" ''hs.dsp.focus({ workspace = "e-1" })'')
          (modBind "mouse_down" ''hs.dsp.focus({ workspace = "e+1" })'')

          (modBind "A" ''hl.dsp.exec_cmd("makoctl dismiss")'')
          (modBind "SHIFT + A" ''hl.dsp.exec_cmd("makoctl dismiss -a")'')
          (modBind "S" ''hl.dsp.exec_cmd("makoctl mode -s do-not-disturb && pkill -SIGRTMIN+1 waybar")'')
          (modBind "SHIFT + S" ''hl.dsp.exec_cmd("makoctl mode -s default && pkill -SIGRTMIN+1 waybar")'')
          (modBind "X" ''hl.dsp.exec_cmd("loginctl lock-session")'')
          (modBind "SHIFT + X" ''hl.dsp.exec_cmd("sleep 2 && systemctl suspend")'')
          (modBind "B" ''hl.dsp.exec_cmd("woomer")'')

          (mkBind "XF86AudioPause" ''hl.dsp.exec_cmd("playerctl play-pause")'')
          (mkBind "XF86AudioPlay" ''hl.dsp.exec_cmd("playerctl play-pause")'')
          (mkBind "XF86AudioStop" ''hl.dsp.exec_cmd("playerctl stop")'')
          (mkBind "XF86AudioPrev" ''hl.dsp.exec_cmd("playerctl previous")'')
          (mkBind "XF86AudioNext" ''hl.dsp.exec_cmd("playerctl next")'')
          (mkBind "XF86AudioMute" ''hl.dsp.exec_cmd("pamixer -t")'')
          (mkBind "XF86AudioMicMute" ''hl.dsp.exec_cmd("pamixer --default-source -t")'')
        ] ++ builtins.concatMap
          (x: [
            (modBind "${toString (lib.mod x 10)}"
              "hs.dsp.focus({ workspace = ${toString x} })")
            (modBind "SHIFT + ${toString (lib.mod x 10)}"
              "hs.dsp.window.move({ workspace = ${toString x}, follow = false })")
          ])
          (lib.range 1 10);
      };
    };
  };
}
