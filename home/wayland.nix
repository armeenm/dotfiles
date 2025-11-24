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

in {
  wayland = lib.optionalAttrs hostPlatform.isLinux {
    windowManager.hyprland = {
      enable = !isHeadless;

      package = if isStandalone then hyprPkgs.hyprland else osConfig.programs.hyprland.package;
      portalPackage = if isStandalone then hyprPkgs.xdg-desktop-portal-hyprland else osConfig.programs.hyprland.portalPackage;

      systemd = {
        enable = isStandalone;
        variables = ["--all"];
      };

      plugins = let
        system = pkgs.stdenv.hostPlatform.system;
        hyprland-plugins = inputs.hyprland-plugins.packages.${system};
      in [
        hyprland-plugins.xtra-dispatchers
        inputs.hypr-darkwindow.packages.${system}.Hypr-DarkWindow
        inputs.hypr-dynamic-cursors.packages.${system}.default
        #inputs.hyprspace.packages.${system}.Hyprspace
        inputs.hyprsplit.packages.${system}.hyprsplit
      ];

      settings = {
        monitor = [ ", preferred, auto, 1, bitdepth, 10" ];

        animation = [
          "global, 1, 2, default"
          "specialWorkspace, 0"
        ];

        decoration = {
          rounding = 0;
          blur.enabled = !isPortable;
          shadow.enabled = !isPortable;
        };

        input = {
          repeat_rate = 60;
          repeat_delay = 200;
          follow_mouse = true;
          touchpad.natural_scroll = true;
          sensitivity = 0;
          scroll_factor = 1.0;
        };

        general = {
          gaps_in = 10;
          gaps_out = "10,20,20,20";
          border_size = 2;
          resize_on_border = true;
          layout = "dwindle";
        };

        env = [
          "HYPRCURSOR_THEME, rose-pine-hyprcursor"
        ];

        dwindle = {
          pseudotile = true;
          preserve_split = true;
        };

        windowrulev2 = [
          "idleinhibit fullscreen, fullscreen:1"
          "stayfocused, class:(zoom), initialTitle:(menu window)"
        ];

        gesture = [
          "3, horizontal, workspace"
        ];

        group = {
          groupbar = {
            font_size = 18;
            gradients = true;
          };
        };

        plugin = {
          dynamic-cursors = {
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

        bindm = [
          "SUPER, mouse:272, movewindow"
          "SUPER, mouse:273, resizewindow"
        ];

        binde = let
          backlight = pkgs.writeShellScript "hyprland-backlight" ''
            set -euo pipefail
            brightnessctl s "$1"
            brightnessctl -m | awk -F',' '{print substr($4, 1, length($4)-1)}' > "$XDG_RUNTIME_DIR"/wob.sock
          '';

          volume = pkgs.writeShellScript "hyprland-volume" ''
            set -euo pipefail
            wpctl set-volume @DEFAULT_AUDIO_SINK@ "$@"
            wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{ print $2*100 }' > "$XDG_RUNTIME_DIR"/wob.sock
          '';
        in [
          ",XF86AudioRaiseVolume,  exec, ${volume} -l 1.5 5%+"
          ",XF86AudioLowerVolume,  exec, ${volume} 5%-"
          ",XF86MonBrightnessUp,   exec, ${backlight} +5%"
          ",XF86MonBrightnessDown, exec, ${backlight} 5%-"

          "SUPER_ALT, H, resizeactive, -30 0"
          "SUPER_ALT, J, resizeactive, 0 30"
          "SUPER_ALT, K, resizeactive, 0 -30"
          "SUPER_ALT, L, resizeactive, 30 0"
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

          screenshot = x: ''hyprshot -zm ${x} -r - | satty -f - -o ~/ss/satty-$(date '+%Y%m%d-%H:%M:%S').png'';

        in [
          "SUPER, Q,         killactive,"
          "SUPER, backspace, forcerendererreload"
          "SUPER, space,     togglefloating,"
          "SUPER, D,         exec, exec $(tofi-run)"
          "SUPER, P,         exec, emacsclient -c -n"
          #"SUPER, grave,     overview:toggle"
          "SUPER, F,         fullscreen, 1"

          "SUPER_SHIFT, return, exec,footclient"
          "SUPER_SHIFT, Q,      killactive,"
          "SUPER_SHIFT, equal,  exit,"
          "SUPER_SHIFT, space,  pin"
          "SUPER_SHIFT, D,      exec,exec $(tofi-drun)"
          "SUPER_SHIFT, P,      pseudo,"
          "SUPER_SHIFT, F,      fullscreen,0"

          "SUPER,       C, exec, ${screenshot "region"}"
          "SUPER_SHIFT, C, exec, ${screenshot "window"}"
          "SUPER_CTRL,  C, exec, ${screenshot "output"}"

          "SUPER,       V, exec, ${screenrec} region"
          "SUPER_SHIFT, V, exec, ${screenrec} window"
          "SUPER_CTRL,  V, exec, ${screenrec} screen"
          "SUPER_ALT,   V, exec, pkill -SIGINT wl-screenrec"

          "SUPER,       W, focusmonitor, l"
          "SUPER,       E, focusmonitor, r"
          "SUPER_SHIFT, W, movewindow, mon:l"
          "SUPER_SHIFT, E, movewindow, mon:r"
          "SUPER_ALT,   W, movecurrentworkspacetomonitor, l"
          "SUPER_ALT,   E, movecurrentworkspacetomonitor, r"

          "SUPER, tab, exec, hyprswitch gui --mod-key super --key tab --close mod-key-release --reverse-key=key=grave --sort-recent && hyprswitch dispatch"

          "SUPER_SHIFT, Z, togglesplit"
          "SUPER, Z, swapsplit"
          "SUPER, T, togglegroup"
          "SUPER, I, invertactivewindow"

          "SUPER, N, changegroupactive,b"
          "SUPER, M, changegroupactive,f"

          "SUPER, H, movefocus,l"
          "SUPER, J, movefocus,d"
          "SUPER, K, movefocus,u"
          "SUPER, L, movefocus,r"

          "SUPER_SHIFT, H, movewindow,l"
          "SUPER_SHIFT, J, movewindow,d"
          "SUPER_SHIFT, K, movewindow,u"
          "SUPER_SHIFT, L, movewindow,r"

          "SUPER,       minus, togglespecialworkspace"
          "SUPER_SHIFT, minus, movetoworkspacesilent,special"

          "SUPER_CTRL, J, split:workspace,e-1"
          "SUPER_CTRL, K, split:workspace,e+1"

          "SUPER, mouse_up, split:workspace,e-1"
          "SUPER, mouse_down, split:workspace,e+1"

          "SUPER,       A, exec, makoctl dismiss"
          "SUPER_SHIFT, A, exec, makoctl dismiss -a"
          "SUPER,       S, exec, makoctl mode -s do-not-disturb && pkill -SIGRTMIN+1 waybar"
          "SUPER_SHIFT, S, exec, makoctl mode -s default && pkill -SIGRTMIN+1 waybar"
          "SUPER,       X, exec, loginctl lock-session"
          "SUPER_SHIFT, X, exec, sleep 2 && systemctl suspend"
          "SUPER,       B, exec, woomer"

          ",XF86AudioPause,   exec, playerctl play-pause"
          ",XF86AudioPlay,    exec, playerctl play-pause"
          ",XF86AudioStop,    exec, playerctl stop"
          ",XF86AudioPrev,    exec, playerctl previous"
          ",XF86AudioNext,    exec, playerctl next"
          ",XF86AudioMute,    exec, pamixer -t"
          ",XF86AudioMicMute, exec, pamixer --default-source -t"
        ] ++ builtins.concatMap
          (x: [
            "SUPER,       ${toString (lib.mod x 10)}, split:workspace, ${toString x}"
            "SUPER_SHIFT, ${toString (lib.mod x 10)}, split:movetoworkspacesilent, ${toString x}"
          ])
          (lib.range 1 10);
      };
    };
  };
}
