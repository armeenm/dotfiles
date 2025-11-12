{ config, pkgs, lib, inputs, root, user, ... }:

{
  nixpkgs.hostPlatform = "aarch64-darwin";
  # This machine uses Determinate.
  nix.enable = lib.mkForce false;

  users.users.${user.login} = {
    name = "armeen";
    home = "/Users/armeen";
  };

  services = {
    emacs = {
      enable = true;
      package = config.home-manager.users.${user.login}.programs.emacs.package;
    };
  };

  home-manager = {
    users.${user.login} = {
      home.stateVersion = lib.mkForce "25.11";
    };

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

  system = {
    stateVersion = 6;
    primaryUser = "armeen";

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToEscape = true;
      swapLeftCtrlAndFn = true;
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
        FXPreferredViewStyle = "Nlsv"; # List view
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
    };
  };
}
