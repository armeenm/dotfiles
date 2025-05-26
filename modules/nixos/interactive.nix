{ inputs, user, root, config, pkgs, ... }:

{
  imports = [ ../shared/stylix.nix ];

  boot = {
    initrd = {
      systemd = {
        enable = true;
        network.enable = true;
      };
    };

    loader = {
      efi.canTouchEfiVariables = true;

      systemd-boot = {
        enable = true;
        editor = false;
      };
    };

    supportedFilesystems = [ "ntfs" "nfs" "nfs4" ];
    binfmt.emulatedSystems = [ "aarch64-linux" ];
    consoleLogLevel = 0;
    extraModulePackages = with config.boot.kernelPackages; [ ddcci-driver ];
    initrd.verbose = false;
  };

  environment.pathsToLink = [
    "/share/applications"
    "/share/xdg-desktop-portal"
  ];

  hardware = {
    sane = {
      enable = true;
      extraBackends = with pkgs; [
        sane-airscan
      ];
    };

    graphics = {
      enable = true;
      enable32Bit = true;
    };
  };

  home-manager = {
    users."${user.login}" = import "${root}/home";

    extraSpecialArgs = {
      inherit inputs root user;
      stateVersion = config.system.stateVersion;
      isHeadless = false;
      isStandalone = false;
      isPortable = false;
      enableSocial = true;
    };
  };

  networking = {
    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };
  };

  programs = {
    adb.enable = true;
    dconf.enable = true;
    nix-ld.enable = true;

    hyprland = let
      hyprPkgs = inputs.hyprland.packages.${pkgs.system};
    in {
      enable = true;
      withUWSM = true;
      package = hyprPkgs.hyprland;
      portalPackage = hyprPkgs.xdg-desktop-portal-hyprland;
    };
  };

  security = {
    pam = {
      u2f.enable = true;

      services = {
        doas = {
          fprintAuth = true;
          u2fAuth = true;
        };

        hyprlock = {
          fprintAuth = true;
          u2fAuth = true;
        };

        login = {
          fprintAuth = true;
          u2fAuth = true;
        };
      };
    };
  };

  services = {
    gnome.gnome-keyring.enable = true;
    gvfs.enable = true;
    mozillavpn.enable = true;
    saned.enable = true;
    upower.enable = true;

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    printing = {
      enable = true;
      drivers = with pkgs; [
        canon-cups-ufr2
        cnijfilter2
        gutenprint
        gutenprintBin
      ];
    };

    udev = {
      packages = with pkgs; [
        ledger-udev-rules
        vial
        yubikey-personalization
      ];
    };
  };

  systemd = {
    tmpfiles.rules = [
      "d /var/srv 0755 - - -"
      "L /srv - - - - /var/srv"
    ];
  };

  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
}
