{ isHeadless
, pkgs
, inputs
, ...
}:

let
  left = "DP-2";
  right = "DP-1";

  hyprland-plugins = inputs.hyprland-plugins.packages.${pkgs.system};

in {
  wayland = {
    windowManager.hyprland = {
      enable = !isHeadless;
      package = null;
      systemd.enable = false;

      plugins = [
        hyprland-plugins.hyprscrolling
        hyprland-plugins.hyprexpo
        hyprland-plugins.xtra-dispatchers
        inputs.hyprsplit.packages.${pkgs.system}.hyprsplit
        inputs.hypr-darkwindow.packages.${pkgs.system}.Hypr-DarkWindow
      ];

      settings = {
        decoration.rounding = 0;
        animations.enabled = false;

        monitor = [
          "${left},highrr,0x525,1"
          "${right},preferred,2560x0,1"
          ",preferred,auto,1"
        ];

        input = {
          kb_options = "caps:escape";
          repeat_rate = 60;
          repeat_delay = 200;
          follow_mouse = true;
          touchpad.natural_scroll = true;
          sensitivity = 0;
        };

        general = {
          gaps_in = 5;
          gaps_out = 10;
          border_size = 2;
          resize_on_border = true;
          layout = "dwindle";
          #"col.active_border" = "rgba(9f40ffcc)";
          #"col.inactive_border" = "rgba(0b0e1411)";
        };

        env = [
          "HYPRCURSOR_THEME,rose-pine-hyprcursor"
        ];

        dwindle = {
          pseudotile = true;
          preserve_split = true;
        };

        workspace = [
          "1, monitor:${left}"
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
          hyprexpo = {
            columns = 3;
            gap_size = 5;
            workspace_method = "center current";

            enable_gesture = true;
            gesture_fingers = 3;
            gesture_distance = 300;
            gesture_positive = true;
          };

          hyprscrolling = {
            fullscreen_on_one_column = true;
          };
        };

        misc = {
          disable_hyprland_logo = true;
          disable_splash_rendering = true;
          font_family = "Tamsyn";
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
          "SUPER,D,exec,systemd-run --user $(tofi-run)"
          "SUPER_SHIFT,D,exec,systemd-run --user $(tofi-drun)"
          "SUPER,P,exec,systemd-run --user emacsclient -c -n"
          "SUPER,grave,hyprexpo:expo,toggle"
          "SUPER_SHIFT,P,pseudo,"
          "SUPER,F,fullscreen,1"
          "SUPER_SHIFT,F,fullscreen,0"

          ''SUPER,C,exec,hyprshot -zm region -r - | satty -f - --fullscreen -o ~/ss/satty-$(date '+%Y%m%d-%H:%M:%S').png''
          ''SUPER_SHIFT,C,exec,hyprshot -zm window -r - | satty -f - --fullscreen -o ~/ss/satty-$(date '+%Y%m%d-%H:%M:%S').png''

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
          "SUPER_SHIFT,Z,togglesplit"
          "SUPER,Z,swapsplit"
          "SUPER,T,togglegroup"
          "SUPER,X,movecursortocorner,0"
          "SUPER,V,invertactivewindow"

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

          "SUPER_CTRL,H,layoutmsg,movewindowto l"
          "SUPER_CTRL,J,layoutmsg,movewindowto d"
          "SUPER_CTRL,K,layoutmsg,movewindowto u"
          "SUPER_CTRL,L,layoutmsg,movewindowto r"

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

          "SUPER,mouse_down,workspace,m+1"
          "SUPER,mouse_up,workspace,m-1"

          "SUPER,A,exec,makoctl dismiss"
          "SUPER_SHIFT,A,exec,makoctl dismiss -a"
          "SUPER,S,exec,makoctl mode -s do-not-disturb"
          "SUPER_SHIFT,S,exec,makoctl mode -s default"
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
      };
    };
  };
}
