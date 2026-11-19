{ lib, inputs, user, config, pkgs, ... }:

{
  imports = [ ../shared/stylix.nix ];

  boot = {
    loader = {
      efi.canTouchEfiVariables = true;

      systemd-boot = {
        enable = true;
        editor = false;
      };
    };

    supportedFilesystems = [ "ntfs" "nfs" "nfs4" ];
    binfmt = {
      emulatedSystems = [
        "aarch64-linux"
        "riscv64-linux"
      ];
      # Load the emulator into the kernel at registration time instead of
      # resolving /run/binfmt inside each build sandbox.
      preferStaticEmulators = true;
    };
    consoleLogLevel = 0;
    extraModulePackages = with config.boot.kernelPackages; [ ddcci-driver ];
  };

  environment = {
    pathsToLink = [
      "/share/applications"
      "/share/xdg-desktop-portal"
    ];
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

  home-manager.extraSpecialArgs = {
    isHeadless = false;
  };

  networking = {
    networkmanager = {
      enable = true;
      wifi.backend = "wpa_supplicant";
    };
  };

  programs = {
    dconf.enable = true;
    nix-ld.enable = true;
    partition-manager.enable = true;

    hyprland = let
      hyprPkgs = inputs.hyprland.packages.${pkgs.system};
    in {
      enable = true;
      withUWSM = true;
      package = hyprPkgs.hyprland;
      portalPackage = hyprPkgs.xdg-desktop-portal-hyprland;
    };

    wireshark = {
      enable = true;
      usbmon.enable = true;
      package = pkgs.wireshark;
    };
  };

  security = {
    pam = {
      # Fingerprint and U2F authentication are opt-in per service. Enabling
      # them globally adds the modules to services that cannot show a prompt,
      # where they just block until they time out.
      services = {
        hyprlock = {
          fprintAuth = true;
          u2f.enable = true;
        };

        login = {
          fprintAuth = true;
          u2f.enable = true;
        };

        polkit-1.fprintAuth = true;

        sudo.fprintAuth = true;
      };
    };
  };

  services = {
    fprintd = {
      enable = true;

      # fprintAuth defaults to this, which puts pam_fprintd (and its 30 second
      # timeout) in front of the password in every PAM service. cupsd
      # re-authenticates each web UI request, so the admin pages stalled until
      # the sensor timed out; sudo, polkit and the rest waited too.
      enablePam = false;
    };

    gnome.gnome-keyring.enable = true;
    gvfs.enable = true;
    mozillavpn.enable = true;
    saned.enable = true;
    upower.enable = true;

    # Only carbon runs an NFS server, so it is the only host that needs the
    # portmapper; NFSv4 clients talk to port 2049 directly. The NFS module
    # turns rpcbind on for any host with nfs in supportedFilesystems.
    rpcbind.enable = lib.mkForce false;

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

  virtualisation = {
    podman = {
      enable = true;
      dockerCompat = true;
      defaultNetwork.settings.dns_enabled = true;
    };
  };

  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
}
