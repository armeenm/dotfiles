{ config, pkgs, lib, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ./home
  ];

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes ca-derivations
    '';
  };

  boot = {
    initrd.availableKernelModules = [ "ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci" ];

    kernelModules = [ "kvm-intel" ];

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/5e1e6d38-5164-417e-b69d-1b538521839a";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/1B30-BFD7";
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

  security = {
    sudo.enable = false;
    doas = {
      enable = true;
      extraRules = [{
        groups = [ "wheel" ];
	keepEnv = true;
      }];
    };
  };

  services = {
    avahi.enable = true;
    blueman.enable = true;
    openssh.enable = true;
    pcscd.enable = true;
    upower.enable = true;

    printing = {
      enable = true;
      drivers = with pkgs; [
        cnijfilter2
        gutenprint
        gutenprintBin
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
    bluetooth.enable = false;
    cpu.intel.updateMicrocode = true;
    pulseaudio.enable = true;

    opengl = {
      enable = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        intel-media-driver
        libvdpau-va-gl
        vaapiIntel
        vaapiVdpau
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

  users.users.armeen = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "adbusers" ];
  };

  environment = {
    defaultPackages = lib.mkForce [ ];
    systemPackages = with pkgs; [
      hdparm
      lm_sensors
      lshw
      pciutils
      usbutils

      git
      rsync
    ];

    variables.EDITOR = "nvim";
    pathsToLink = [ "/share/zsh" ];
  };

  programs = {
    adb.enable = true;
    light.enable = true;

    neovim = {
      enable = true;
      defaultEditor = true;
      viAlias = true;
      vimAlias = true;
      configure = {
        customRC = ''
          set number
          set hidden
          set shell=bash
          set cmdheight=2
          set nocompatible
          set shortmess+=c
          set updatetime=300
          set background=dark
          set foldmethod=marker
          set signcolumn=yes
          set nobackup nowritebackup
          set tabstop=2 shiftwidth=2 expandtab
          set tagrelative
          set tags^=./.git/tags;
          set mouse=a
        '';
      };
    };
  };

  system.stateVersion = lib.mkForce "21.11";
}
