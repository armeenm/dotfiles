{ config, sys, pkgs, lib, root, user, inputs, ... }:

let
  left = "DP-2";
  right = "DP-1";

in {
  wayland = {
    windowManager.hyprland = {
      enable = true;
      settings = {
        monitor = [
          "${left},highrr,0x525,1"
          "${right},preferred,2560x0,1"
          ",preferred,auto,1"
        ];

        workspace = "1,monitor:${left}";

        input = {
          kb_options = "caps:escape";

          repeat_rate = 60;
          repeat_delay = 200;

          follow_mouse = true;

          touchpad.natural_scroll = true;

          sensitivity = 0; # -1.0 - 1.0, 0 means no modification.
        };

        general = {
          gaps_in = 5;
          gaps_out = 10;
          border_size = 2;
          "col.active_border" = "rgba(9f40ffcc)";
          "col.inactive_border" = "rgba(0b0e1411)";
        };

        decoration.rounding = 0;
        animations.enabled = false;

        dwindle = {
          pseudotile = true;
          no_gaps_when_only = true;
          preserve_split = true;
        };

        gestures = {
          workspace_swipe = true;
        };

        misc = {
          disable_hyprland_logo = true;
          disable_splash_rendering = true;
        };

        bindm = [
          "SUPER,mouse:272,movewindow"
          "SUPER,mouse:273,resizewindow"
        ];

        bind = [
          "SUPER_SHIFT,return,exec,systemd-run --user footclient"
          "SUPER,Q,killactive,"
          "SUPER_SHIFT,backspace,exit,"
          "SUPER,backspace,forcerendererreload"
          "SUPER,space,togglefloating,"
          "SUPER_SHIFT,space,pin"
          "SUPER,D,exec,bemenu-run -b"
          "SUPER,P,exec,systemd-run --user emacsclient -c -n"
          ''SUPER,grave,exec,grim -g "$(slurp)" - | swappy -f -''
          "SUPER_SHIFT,P,pseudo,"
          "SUPER,F,fullscreen,1"
          "SUPER_SHIFT,F,fullscreen,0"

          "SUPER,W,focusmonitor,${left}"
          "SUPER,E,focusmonitor,${right}"
          "SUPER_SHIFT,W,movewindow,mon:${left}"
          "SUPER_SHIFT,E,movewindow,mon:${right}"
          "SUPER_ALT,W,movecurrentworkspacetomonitor,${left}"
          "SUPER_ALT,E,movecurrentworkspacetomonitor,${right}"

          "SUPER_ALT,H,resizeactive,-30 0"
          "SUPER_ALT,J,resizeactive,0 30"
          "SUPER_ALT,K,resizeactive,0 -30"
          "SUPER_ALT,L,resizeactive,30 0"

          "SUPER,tab,workspace,previous"
          "SUPER,minus,togglespecialworkspace"
          "SUPER,Z,togglesplit"
          "SUPER_SHIFT,Z,swapsplit"
          "SUPER,T,togglegroup"
          "SUPER,X,movecursortocorner,0"

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

          "SUPER,1,workspace,1"
          "SUPER,2,workspace,2"
          "SUPER,3,workspace,3"
          "SUPER,4,workspace,4"
          "SUPER,5,workspace,5"
          "SUPER,6,workspace,6"
          "SUPER,7,workspace,7"
          "SUPER,8,workspace,8"
          "SUPER,9,workspace,9"
          "SUPER,0,workspace,10"

          "SUPER_SHIFT,1,movetoworkspacesilent,1"
          "SUPER_SHIFT,2,movetoworkspacesilent,2"
          "SUPER_SHIFT,3,movetoworkspacesilent,3"
          "SUPER_SHIFT,4,movetoworkspacesilent,4"
          "SUPER_SHIFT,5,movetoworkspacesilent,5"
          "SUPER_SHIFT,6,movetoworkspacesilent,6"
          "SUPER_SHIFT,7,movetoworkspacesilent,7"
          "SUPER_SHIFT,8,movetoworkspacesilent,8"
          "SUPER_SHIFT,9,movetoworkspacesilent,9"
          "SUPER_SHIFT,0,movetoworkspacesilent,10"
          "SUPER_SHIFT,minus,movetoworkspacesilent,special"

          "SUPER,mouse_down,workspace,m+1"
          "SUPER,mouse_up,workspace,m-1"

          "SUPER,A,exec,makoctl dismiss"
          "SUPER_SHIFT,A,exec,makoctl dismiss -a"
          "SUPER,S,exec,makoctl set-mode do-not-disturb"
          "SUPER_SHIFT,S,exec,makoctl set-mode default"
          "SUPER_SHIFT,X,exec,hyprlock"

          ",xf86audiopause,exec,playerctl play-pause"
          ",xf86audioplay,exec,playerctl play-pause"
          ",xf86audiostop,exec,playerctl stop"
          ",xf86audioprev,exec,playerctl previous"
          ",xf86audionext,exec,playerctl next"
          ",xf86monbrightnessup,exec,light -A 5"
          ",xf86monbrightnessdown,exec,light -U 5"
          ",xf86audioraisevolume,exec,pamixer -i 5"
          ",xf86audiolowervolume,exec,pamixer -d 5"
          ",xf86audiomute,exec,pamixer -t"
          ",xf86audiomicmute,exec,pamixer --default-source -t"
        ];

        # TODO: Make these units wanted-by's for the hyprland target.
        exec-once = [
          "systemctl --user start foot waybar emacs easyeffects"
        ];
      };
    };
  };
}
