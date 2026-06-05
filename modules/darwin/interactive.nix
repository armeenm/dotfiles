{ config, user, ... }:

{
  imports = [ ../shared/stylix.nix ];

  environment.variables = {
    SSH_SK_PROVIDER = "/usr/lib/ssh-keychain.dylib";
  };

  home-manager = {
    extraSpecialArgs = {
      isHeadless = false;
    };
  };

  security = {
    pam.services.sudo_local = {
      enable = true;
      touchIdAuth = true;
    };
  };

  services = {
    aerospace = {
      enable = true;
      settings = let
        hmConfig = config.home-manager.users.${user.login};
        launch = x: "exec-and-forget open -n -a ${x}";
      in {
        automatically-unhide-macos-hidden-apps = true;
        after-startup-command = [ "exec-and-forget sketchybar" ];
        exec-on-workspace-change = [
          "/bin/bash" "-c" "sketchybar --trigger aerospace_workspace_change FOCUSED_WORKSPACE=$AEROSPACE_FOCUSED_WORKSPACE"
        ];
        on-focus-changed = [ "move-mouse window-lazy-center" ];
        on-window-detected = [
          {
            "if".app-id = "net.whatsapp.WhatsApp";
            run = [ "layout tiling" ];
          }
          {
            "if".app-id = "org.alacritty";
            run = [ "layout tiling" ];
          }
        ];

        mode.main.binding = {
          alt-h = "focus --boundaries all-monitors-outer-frame left";
          alt-j = "focus --boundaries all-monitors-outer-frame down";
          alt-k = "focus --boundaries all-monitors-outer-frame up";
          alt-l = "focus --boundaries all-monitors-outer-frame right";

          alt-ctrl-h = "join-with left";
          alt-ctrl-j = "join-with down";
          alt-ctrl-k = "join-with up";
          alt-ctrl-l = "join-with right";

          alt-shift-h = "move --boundaries all-monitors-outer-frame left";
          alt-shift-j = "move --boundaries all-monitors-outer-frame down";
          alt-shift-k = "move --boundaries all-monitors-outer-frame up";
          alt-shift-l = "move --boundaries all-monitors-outer-frame right";

          alt-1 = "workspace 1";
          alt-2 = "workspace 2";
          alt-3 = "workspace 3";
          alt-4 = "workspace 4";
          alt-5 = "workspace 5";
          alt-6 = "workspace 6";
          alt-7 = "workspace 7";
          alt-8 = "workspace 8";
          alt-9 = "workspace 9";

          alt-shift-1 = "move-node-to-workspace 1";
          alt-shift-2 = "move-node-to-workspace 2";
          alt-shift-3 = "move-node-to-workspace 3";
          alt-shift-4 = "move-node-to-workspace 4";
          alt-shift-5 = "move-node-to-workspace 5";
          alt-shift-6 = "move-node-to-workspace 6";
          alt-shift-7 = "move-node-to-workspace 7";
          alt-shift-8 = "move-node-to-workspace 8";
          alt-shift-9 = "move-node-to-workspace 9";

          alt-tab = "workspace-back-and-forth";
          alt-space = "layout floating tiling";
          alt-f = "fullscreen";
          alt-r = "mode resize";
          alt-p = "exec-and-forget ${hmConfig.programs.emacs.finalPackage}/bin/emacsclient -c -n";
          alt-shift-enter = ''exec-and-forget ${hmConfig.programs.alacritty.package}/bin/alacritty msg create-window'';
        };

        mode.resize.binding = {
          esc = "mode main";
          enter = "mode main";
          backspace = "flatten-workspace-tree";
          b = "balance-sizes";

          h = "resize width -50";
          j = "resize height -50";
          k = "resize height +50";
          l = "resize width +50";
        };
      };
    };

    sketchybar = {
      enable = true;
      config = builtins.readFile ../../conf/sketchybarrc;
    };
  };

  system = {
    primaryUser = user.login;

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToEscape = true;
      # swapLeftCtrlAndFn = true;
      # swapLeftCommandAndLeftAlt = true;
    };

    defaults = {
      ActivityMonitor.IconType = 5;

      NSGlobalDomain = {
        AppleInterfaceStyleSwitchesAutomatically = true;
        AppleKeyboardUIMode = 3;
        InitialKeyRepeat = 10;
        KeyRepeat = 1;
      };

      SoftwareUpdate.AutomaticallyInstallMacOSUpdates = false;

      controlcenter = {
        BatteryShowPercentage = true;
        Bluetooth = true;
        Sound = true;
      };

      dock = {
        autohide = true;
        wvous-bl-corner = 3; # Application Windows
        wvous-br-corner = 4; # Desktop
        wvous-tr-corner = 7; # Dashboard
      };

      finder = {
        ShowPathbar = true;
        ShowStatusBar = true;
        FXPreferredViewStyle = "Nlsv"; # List view.
        _FXShowPosixPathInTitle = false;
        AppleShowAllExtensions = true;
      };

      screencapture = {
        location = "~/Pictures/Screenshots";
        type = "png";
      };

      trackpad = {
        Clicking = true;
      };

      menuExtraClock = {
        ShowSeconds = true;
      };

      CustomSystemPreferences = {
        NSGlobalDomain = {
          NSWindowShouldDragOnGesture = true;
        };
      };
    };
  };
}
