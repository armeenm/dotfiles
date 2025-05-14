{ config, pkgs, lib, modulesPath, inputs, root, user, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/81BA-80F5";
      fsType = "vfat";
      options = [ "fmask=0077" "dmask=0077" ];
    };

    "/" = {
      device = "/dev/disk/by-uuid/98affdf5-1020-4752-994e-343092303139";
      fsType = "ext4";
    };
  };

  boot = {
    binfmt.emulatedSystems = [ "aarch64-linux" ];
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
    kernelModules = [ "kvm-intel" ];
    supportedFilesystems = [ "ntfs" ];

    loader = {
      efi.canTouchEfiVariables = true;

      systemd-boot = {
        enable = true;
        editor = false;
      };
    };
  };

  networking = {
    hostName = "argentum";
    wireless.iwd.enable = true;
  };

  hardware = {
    enableAllFirmware = true;
    bluetooth.enable = true;
    cpu.intel.updateMicrocode = true;
    sensor.iio.enable = true;

    graphics = {
      enable = true;
      enable32Bit = true;
    };

    sane = {
      enable = true;
      extraBackends = with pkgs; [
        sane-airscan
      ];
    };
  };

  nixpkgs = {
    hostPlatform = "x86_64-linux";
  };

  services = {
    blueman.enable = true;
    flatpak.enable = true;
    saned.enable = true;

    hardware = {
      bolt.enable = true;
    };

    logind = {
      extraConfig = "HandlePowerKey=suspend";
      lidSwitch = "suspend";
    };

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

    displayManager = {
      defaultSession = "plasma";
      sddm = {
        enable = true;
        wayland.enable = true;
      };
    };

    xserver = {
      enable = true;
      videoDrivers = [ "intel" ];
      desktopManager.plasma6.enable = true;
    };
  };

  systemd = {
    tmpfiles.rules = [
      "d /var/srv 0755 - - -"
      "L /srv - - - - /var/srv"
    ];
  };

  security = {
    acme = {
      acceptTerms = true;
      defaults.email = user.email;
    };

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
  };

  users.users."${user.login}".extraGroups = [
    "adbusers"
    "i2c"
    "lp" # Printing
    "plugdev"
    "scanner"
  ];

  home-manager = {
    users = {
      "${user.login}" = import "${root}/home";
      arash = import "${root}/home";
    };

    extraSpecialArgs = {
      inherit inputs root user;
      stateVersion = config.system.stateVersion;
      isHeadless = false;
    };
  };

  programs = {
    adb.enable = true;
    dconf.enable = true;
    light.enable = true;

    hyprland = let
      hyprPkgs = inputs.hyprland.packages.${pkgs.system};
    in {
      enable = true;
      withUWSM = true;
      package = hyprPkgs.hyprland;
      portalPackage = hyprPkgs.xdg-desktop-portal-hyprland;
    };
  };

  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  system.stateVersion = lib.mkForce "24.11";
}
