{ config, pkgs, lib, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  system.stateVersion = lib.mkForce "21.05";

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes
    '';
  };

  boot = {
    kernelModules = [ "kvm-intel" ];

    initrd.availableKernelModules = [ "ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci" ];

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    kernel.sysctl."user.max_user_namespaces" = 28633;
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/990c3938-5b91-47e4-adb9-d5104e58b616";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/EFA4-F842";
      fsType = "vfat";
    };
  };

  time.timeZone = "America/Chicago";

  networking = {
    hostName = "cesium";
    networkmanager.enable = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    keyMap = "us";
    font = "Tamsyn6x12r";
    packages = [ pkgs.tamsyn ];
    earlySetup = true;
  };

  services = {
    avahi.enable = true;
    blueman.enable = true;
    ipfs.enable = false;
    #mullvad-vpn.enable = true;
    openssh.enable = true;
    pcscd.enable = true;
    upower.enable = true;

    printing = {
      enable = true;
      drivers = with pkgs; [
        gutenprint
        gutenprintBin
        cnijfilter2
      ];
    };

    physlock = {
      enable = true;
      allowAnyUser = true;
    };

    udev.packages = with pkgs; [
      ledger-udev-rules
      yubikey-personalization
    ];

    actkbd = {
      enable = true;
      bindings = [
        {
          keys = [ 224 ];
          events = [ "key" ];
          command = "${pkgs.light}/bin/light -U 10";
        }
        {
          keys = [ 225 ];
          events = [ "key" ];
          command = "${pkgs.light}/bin/light -A 10";
        }
      ];
    };

    xserver = {
      enable = true;
      layout = "us";

      libinput = {
        enable = true;
        mouse.accelProfile = "flat";
        mouse.accelSpeed = "0";
        touchpad.accelProfile = "flat";
        touchpad.accelSpeed = "0";
      };

      displayManager.startx.enable = true;

      xkbOptions = "caps:ctrl_modifier";
    };
  };

  hardware = {
    bluetooth.enable = true;
    pulseaudio.enable = true;

    opengl = {
      enable = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        vaapiVdpau
        libvdpau-va-gl
      ];
    };

    trackpoint = {
      enable = true;
      sensitivity = 200;
      speed = 160;
      emulateWheel = true;
    };
  };

  sound = {
    enable = true;
    mediaKeys = {
      enable = true;
      volumeStep = "5%";
    };
  };

  users.users.nixpower = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "adbusers" ];
  };

  environment = {
    defaultPackages = lib.mkForce [ ];

    variables.EDITOR = "nvim";
    pathsToLink = [ "/share/zsh" ];
  };

  programs = {
    light.enable = true;
    adb.enable = true;

    neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
    };
  };
}
