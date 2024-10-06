{ config, pkgs, lib, modulesPath, inputs, root, user, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/BB4A-4BC9";
      fsType = "vfat";
    };

    "/" = {
      device = "/dev/disk/by-uuid/e9453664-465f-44d5-b453-78c9e7e95997";
      fsType = "ext4";
    };
  };

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "usb_storage" "sd_mod" "sdhci_pci" ];
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
    hostName = "boron";
    firewall.enable = true;
    useNetworkd = true;
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
  };

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = null;

  console = {
    keyMap = "us";
    font = "Tamsyn7x13r";
    packages = [ pkgs.tamsyn ];
    earlySetup = false;
  };

  nix = {
    package = pkgs.nixVersions.latest;
    channel.enable = true;
    nixPath = lib.mkForce [ "nixpkgs=${config.nix.registry.nixpkgs.flake}" ];

    registry = {
      nixpkgs.flake = inputs.nixpkgs;
    };

    settings = {
      allowed-users = lib.mkForce [ "@users" "@wheel" ];
      trusted-users = lib.mkForce [ "@wheel" ];

      experimental-features = [
        "auto-allocate-uids"
        "ca-derivations"
        "flakes"
        "nix-command"
        "recursive-nix"
      ];

      warn-dirty = false;
    };
  };

  nixpkgs = {
    hostPlatform = "x86_64-linux";
  };

  services = {
    blueman.enable = true;
    fstrim.enable = true;
    haveged.enable = true;
    physlock.enable = true;
    timesyncd.enable = true;
    udisks2.enable = true;
    fwupd.enable = true;
    xserver.videoDrivers = [ "intel" ];

    avahi = {
      enable = true;
      nssmdns4 = true;
      nssmdns6 = true;
    };

    openssh = {
      enable = true;

      settings = {
        PasswordAuthentication = true; # XXX
      };
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    resolved = {
      enable = true;
      fallbackDns = lib.mkForce [];
      dnssec = "false";
    };
  };

  systemd = {
    watchdog.rebootTime = "15s";

    tmpfiles.rules = [
      "d /var/srv 0755 - - -"
      "L /srv - - - - /var/srv"
    ];

    suppressedSystemUnits = [
      "sys-kernel-debug.mount"
    ];
  };

  security = {
    allowUserNamespaces = true;
    protectKernelImage = true;
    unprivilegedUsernsClone = true;
    virtualisation.flushL1DataCache = null;

    rtkit.enable = true;
    sudo.enable = false;

    audit = {
      enable = false;
      rules = [ ];
    };

    doas = {
      enable = true;
      extraRules = [{
        groups = [ "wheel" ];
        keepEnv = true;
        noPass = true;
      }];
    };

    pam = {
      u2f.enable = true;
      services.hyprlock = {};
    };
  };

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = true;

    users = {
      root.hashedPassword = null;

      "${user.login}" = {
        isNormalUser = true;
        extraGroups = [ "wheel" ];
      };
    };
  };

  home-manager = {
    users = {
      "${user.login}" = import "${root}/home";
    };
    extraSpecialArgs = {
      inherit inputs root user;
      stateVersion = config.system.stateVersion;
    };
  };

  environment = {
    defaultPackages = lib.mkForce [ ];

    systemPackages = (with pkgs; [
      doas-sudo-shim
      efibootmgr
      git
      hdparm
      lm_sensors
      lshw
      pciutils
      rsync
      usbutils
    ]);
  };

  programs = {
    hyprland.enable = true;
    zsh.enable = true;

    neovim = {
      enable = true;
      defaultEditor = true;
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

  zramSwap.enable = true;

  system.stateVersion = lib.mkForce "24.11";
}
