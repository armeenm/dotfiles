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
    extraModulePackages = with config.boot.kernelPackages; [ tp_smapi ];
    initrd.availableKernelModules = [ "ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci" ];

    kernelModules = [ "kvm-intel" "tp_smapi" ];

    kernelParams = [ "i915.enable_rc6=7" ];

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
    interfaces.enp0s25.useDHCP = true;
    wireless.iwd.enable = true;

    openconnect.interfaces = import ./secrets/gc/openconnect.nix;
  };

  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    keyMap = "us";
    font = "Tamsyn6x12r";
    packages = [ pkgs.tamsyn ];
    earlySetup = true;
  };

  security = {
    rtkit.enable = true;
    sudo.enable = false;

    doas = {
      enable = true;
      extraRules = [{
        groups = [ "wheel" ];
	      keepEnv = true;
      }];
    };

    pki.certificateFiles = [ ./secrets/gc/WolframCA3.crt ];
  };

  services = {
    avahi.enable = true;
    blueman.enable = true;
    fstrim.enable = true;
    openssh.enable = true;
    pcscd.enable = true;
    tlp.enable = true;
    upower.enable = true;

    openvpn.servers = {
      wolfram = {
        autoStart = false;
        config = '' config /home/armeen/.config/openvpn/wolfram.conf '';
      };
    };

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

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
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
  };

  hardware = {
    bluetooth.enable = false;
    cpu.intel.updateMicrocode = true;

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
    enable = false;
    mediaKeys = {
      enable = true;
      volumeStep = "5%";
    };
  };

  users.users.armeen = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [
      "wheel"
      "adbusers"
    ];
  };

  environment = {
    defaultPackages = lib.mkForce [ ];
    systemPackages = with pkgs; [
      hdparm
      lm_sensors
      lshw
      pciutils
      usbutils

      pcsctools

      git
      rsync
    ];

    variables.EDITOR = "nvim";
    pathsToLink = [ "/share/zsh" ];

    etc.openvpn.source = "${pkgs.update-resolv-conf}/libexec/openvpn";
  };

  programs = {
    adb.enable = true;
    light.enable = true;
    nix-ld.enable = true;

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

  virtualisation = {
    podman = {
      enable = true;
      dockerCompat = true;
      dockerSocket.enable = true;
      defaultNetwork.dnsname.enable = true;
    };
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
    gtkUsePortal = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
    ];
  };

  system.stateVersion = lib.mkForce "21.11";
}
