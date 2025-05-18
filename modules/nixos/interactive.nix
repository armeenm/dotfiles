{ inputs, user, root, config, pkgs, ... }:

{
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
        swaylock.fprintAuth = true;
        hyprlock.fprintAuth = true;
        login.fprintAuth = true;
        doas.fprintAuth = true;

        swaylock.u2fAuth = true;
        hyprlock.u2fAuth = true;
        login.u2fAuth = true;
        doas.u2fAuth = true;
      };
    };

    krb5 = {
      enable = true;
      settings = {
        libdefaults = {
          default_realm = "ARMEEN.XYZ";
        };

        domain_realm = {
          "armeen.xyz" = "ARMEEN.XYZ";
        };

        realms = {
          "ARMEEN.XYZ" = {
            admin_server = "cobalt.armeen.xyz";
            kdc = [
              "cobalt.armeen.xyz"
            ];
          };
        };
      };
    };
  };

  services = {
    gnome.gnome-keyring.enable = true;
    gvfs.enable = true;
    saned.enable = true;
    mozillavpn.enable = true;

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

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/ayu-mirage.yaml";

    fonts = {
      sizes = {
        desktop = 14;
        popups = 12;
      };

      serif = {
        package = pkgs.crimson;
        name = "Crimson Pro";
      };

      sansSerif = {
        package = pkgs.lato;
        name = "Lato";
      };

      monospace = {
        package = pkgs.tamsyn;
        name = "Tamsyn";
      };
    };

    opacity = {
      applications = 0.8;
      desktop = 0.8;
      popups = 0.5;
      terminal = 0.8;
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
