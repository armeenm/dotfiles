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

  hyprPkgs = inputs.hyprland.packages.${pkgs.system};

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
        hyprland-plugins = inputs.hyprland-plugins.packages.${pkgs.system};
      in [
        hyprland-plugins.hyprscrolling
        hyprland-plugins.xtra-dispatchers
        inputs.hypr-darkwindow.packages.${pkgs.system}.Hypr-DarkWindow
        inputs.hypr-dynamic-cursors.packages.${pkgs.system}.default
        inputs.hyprspace.packages.${pkgs.system}.Hyprspace
        inputs.hyprsplit.packages.${pkgs.system}.hyprsplit
      ];

      settings = {
        misc.vfr = true;
        monitor = [ ",preferred,auto,1" ];

        animation = [
          "global,1,2,default"
          "specialWorkspace,0"
        ];

        decoration = {
          rounding = 0;
          blur.enabled = !isPortable;
          shadow.enabled = !isPortable;
        };

        input = {
          kb_options = "caps:escape";
          repeat_rate = 60;
          repeat_delay = 200;
          follow_mouse = true;
          touchpad.natural_scroll = true;
          sensitivity = 0;
        };

        general = {
          gaps_in = 10;
          gaps_out = "10,20,20,20";
          border_size = 2;
          resize_on_border = true;
          layout = "dwindle";
        };

        env = [
          "HYPRCURSOR_THEME,rose-pine-hyprcursor"
        ];

        dwindle = {
          pseudotile = true;
          preserve_split = true;
        };

        workspace = [
          # "No gaps when only" functionality.
          "w[tv1], gapsout:0, gapsin:0"
          "f[1], gapsout:0, gapsin:0"
        ];

        # "No gaps when only" functionality.
        windowrulev2 = [
          "bordersize 0, floating:0, onworkspace:w[tv1]"
          "rounding 0, floating:0, onworkspace:w[tv1]"
          "bordersize 0, floating:0, onworkspace:f[1]"
          "rounding 0, floating:0, onworkspace:f[1]"
          "float, class:com.gabm.satty"
          "idleinhibit fullscreen, fullscreen:1"
        ];

        gestures = {
          workspace_swipe = true;
        };

        group = {
          groupbar = {
            font_size = 18;
            gradients = true;
          };
        };

        plugin = {
          hyprscrolling.fullscreen_on_one_column = true;

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
          "SUPER,mouse:272,movewindow"
          "SUPER,mouse:273,resizewindow"
        ];

        bind = let
          selectWindow = pkgs.writeShellScript "hyprland-selectwin" ''
            set -euo pipefail
            hyprctl clients -j | jq --argjson active $(hyprctl monitors -j | jq -c '[.[].activeWorkspace.id]') '.[] | select((.hidden | not) and .workspace.id as $id | $active | contains([$id])) | "\(.at[0]),\(.at[1]) \(.size[0])x\(.size[1])"' -r | slurp
          '';

          backlight = pkgs.writeShellScript "hyprland-backlight" ''
            set -euo pipefail
            brightnessctl s "$1"
            brightnessctl -m | awk -F',' '{print substr($4, 1, length($4)-1)}' > "$XDG_RUNTIME_DIR"/wob.sock
          '';

          volume = pkgs.writeShellScript "hyprland-volume" ''
            set -euo pipefail
            pamixer "$@"
            pamixer --get-volume > "$XDG_RUNTIME_DIR"/wob.sock
          '';

        in [
          "SUPER_SHIFT,return,exec,footclient"
          "SUPER,Q,killactive,"
          "SUPER_SHIFT,backspace,exit,"
          "SUPER,backspace,forcerendererreload"
          "SUPER,space,togglefloating,"
          "SUPER_SHIFT,space,pin"
          "SUPER,D,exec,systemd-run --user $(tofi-run)"
          "SUPER_SHIFT,D,exec,systemd-run --user $(tofi-drun)"
          "SUPER,P,exec,emacsclient -c -n"
          "SUPER,grave,overview:toggle"
          "SUPER_SHIFT,P,pseudo,"
          "SUPER,F,fullscreen,1"
          "SUPER_SHIFT,F,fullscreen,0"

          ''SUPER,C,exec,hyprshot -zm region -r - | satty -f - -o ~/ss/satty-$(date '+%Y%m%d-%H:%M:%S').png''
          ''SUPER_SHIFT,C,exec,hyprshot -zm window -r - | satty -f - -o ~/ss/satty-$(date '+%Y%m%d-%H:%M:%S').png''
          ''SUPER_CTRL,C,exec,hyprshot -zm output -r - | satty -f - -o ~/ss/satty-$(date '+%Y%m%d-%H:%M:%S').png''

          ''SUPER,V,exec,wl-screenrec -g "$(slurp)" -f ~/ss/wl-screenrec-$(date '+%Y%m%d-%H:%M:%S').mp4''
          ''SUPER_SHIFT,V,exec,wl-screenrec -g "$(${selectWindow})" -f ~/ss/wl-screenrec-$(date '+%Y%m%d-%H:%M:%S').mp4''
          ''SUPER_CTRL,V,exec,wl-screenrec -g "$(slurp -o)" -f ~/ss/wl-screenrec-$(date '+%Y%m%d-%H:%M:%S').mp4''
          "SUPER_ALT,V,exec,pkill -SIGINT wl-screenrec"

          "SUPER,W,focusmonitor,l"
          "SUPER,E,focusmonitor,r"
          "SUPER_SHIFT,W,movewindow,mon:l"
          "SUPER_SHIFT,E,movewindow,mon:r"
          "SUPER_ALT,W,movecurrentworkspacetomonitor,l"
          "SUPER_ALT,E,movecurrentworkspacetomonitor,r"

          "SUPER_ALT,H,resizeactive,-30 0"
          "SUPER_ALT,J,resizeactive,0 30"
          "SUPER_ALT,K,resizeactive,0 -30"
          "SUPER_ALT,L,resizeactive,30 0"

          "SUPER,tab,exec,hyprswitch gui --mod-key super --key tab --close mod-key-release --reverse-key=key=grave --sort-recent && hyprswitch dispatch"

          "SUPER,minus,togglespecialworkspace"
          "SUPER_SHIFT,Z,togglesplit"
          "SUPER,Z,swapsplit"
          "SUPER,T,togglegroup"
          "SUPER,I,invertactivewindow"

          "SUPER,N,changegroupactive,b"
          "SUPER,M,changegroupactive,f"

          "SUPER,H,movefocus,l"
          "SUPER,J,movefocus,d"
          "SUPER,K,movefocus,u"
          "SUPER,L,movefocus,r"

          "SUPER_SHIFT,H,movewindow,l"
          "SUPER_SHIFT,J,movewindow,d"
          "SUPER_SHIFT,K,movewindow,u"
          "SUPER_SHIFT,L,movewindow,r"

          "SUPER,comma,layoutmsg,move -col"
          "SUPER,period,layoutmsg,move +col"

          "SUPER,1,split:workspace,1"
          "SUPER,2,split:workspace,2"
          "SUPER,3,split:workspace,3"
          "SUPER,4,split:workspace,4"
          "SUPER,5,split:workspace,5"
          "SUPER,6,split:workspace,6"
          "SUPER,7,split:workspace,7"
          "SUPER,8,split:workspace,8"
          "SUPER,9,split:workspace,9"
          "SUPER,0,split:workspace,10"

          "SUPER_SHIFT,1,split:movetoworkspacesilent,1"
          "SUPER_SHIFT,2,split:movetoworkspacesilent,2"
          "SUPER_SHIFT,3,split:movetoworkspacesilent,3"
          "SUPER_SHIFT,4,split:movetoworkspacesilent,4"
          "SUPER_SHIFT,5,split:movetoworkspacesilent,5"
          "SUPER_SHIFT,6,split:movetoworkspacesilent,6"
          "SUPER_SHIFT,7,split:movetoworkspacesilent,7"
          "SUPER_SHIFT,8,split:movetoworkspacesilent,8"
          "SUPER_SHIFT,9,split:movetoworkspacesilent,9"
          "SUPER_SHIFT,0,split:movetoworkspacesilent,10"
          "SUPER_SHIFT,minus,movetoworkspacesilent,special"

          "SUPER_CTRL,J,split:workspace,e-1"
          "SUPER_CTRL,K,split:workspace,e+1"
          "SUPER,mouse_down,split:workspace,e-1"
          "SUPER,mouse_up,split:workspace,e+1"

          "SUPER,A,exec,makoctl dismiss"
          "SUPER_SHIFT,A,exec,makoctl dismiss -a"
          "SUPER,S,exec,makoctl mode -s do-not-disturb && pkill -SIGRTMIN+1 waybar"
          "SUPER_SHIFT,S,exec,makoctl mode -s default && pkill -SIGRTMIN+1 waybar"
          "SUPER,X,exec,loginctl lock-session"
          "SUPER_SHIFT,X,exec,sleep 3 && systemctl suspend"
          "SUPER,B,exec,woomer"

          ",xf86audiopause,exec,playerctl play-pause"
          ",xf86audioplay,exec,playerctl play-pause"
          ",xf86audiostop,exec,playerctl stop"
          ",xf86audioprev,exec,playerctl previous"
          ",xf86audionext,exec,playerctl next"
          ",xf86monbrightnessup,exec,${backlight} +5%"
          ",xf86monbrightnessdown,exec,${backlight} 5%-"
          ",xf86audioraisevolume,exec,${volume} -i 5"
          ",xf86audiolowervolume,exec,${volume} -d 5"
          ",xf86audiomute,exec,pamixer -t"
          ",xf86audiomicmute,exec,pamixer --default-source -t"
        ];
      };
    };
  };
}
