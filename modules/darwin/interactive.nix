{ user, ... }:

{
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

  system = {
    primaryUser = user.login;

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToEscape = true;
      swapLeftCtrlAndFn = true;
      swapLeftCommandAndLeftAlt = true;
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
    };
  };
}
