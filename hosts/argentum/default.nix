{ config, pkgs, lib, modulesPath, inputs, root, user, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/BBE8-0DBE";
      fsType = "vfat";
    };

    "/" = {
      device = "/dev/disk/by-uuid/c8663fee-c299-4e3b-a482-6c19d4f9fcc9";
      fsType = "ext4";
      options = ["noatime"];
    };
  };

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
    kernelModules = [ "kvm-intel" ];

    supportedFilesystems = [ "ntfs" ];
    binfmt.emulatedSystems = [ "aarch64-linux" ];

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
    firewall.enable = true;
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

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = null;

  console = {
    keyMap = "us";
    font = "Tamsyn7x13r";
    packages = [ pkgs.tamsyn ];
    earlySetup = true;
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
    flatpak.enable = true;
    fstrim.enable = true;
    fwupd.enable = true;
    haveged.enable = true;
    physlock.enable = false;
    power-profiles-daemon.enable = false;
    saned.enable = true;
    smartd.enable = true;
    tcsd.enable = false;
    timesyncd.enable = true;
    tlp.enable = true;
    udisks2.enable = true;

    avahi = {
      enable = true;
      nssmdns4 = true;
      nssmdns6 = true;
    };

    hardware = {
      bolt.enable = true;
    };

    openssh = {
      enable = true;

      settings = {
        PasswordAuthentication = false;
      };
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

    displayManager.sddm.enable = true;

    xserver = {
      enable = true;
      videoDrivers = [ "intel" ];
      desktopManager.plasma5.enable = true;
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

    apparmor.enable = true;
    auditd.enable = true;
    rtkit.enable = true;
    polkit.enable = true;
    sudo.enable = false;

    acme = {
      acceptTerms = true;
      defaults.email = user.email;
    };

    audit = {
      enable = false;
      rules = [ ];
    };

    doas = {
      enable = true;
      extraRules = [{
        groups = [ "wheel" ];
        keepEnv = true;
        noPass = false;
      }];
    };

    pam = {
      u2f.enable = true;
      services = {
        #swaylock.fprintAuth = true;
        #hyprlock.fprintAuth = true;
        #login.fprintAuth = true;
        #doas.fprintAuth = true;

        swaylock.u2fAuth = true;
        hyprlock.u2fAuth = true;
        login.u2fAuth = true;
        doas.u2fAuth = true;
      };
    };

    tpm2 = {
      enable = true;
      abrmd.enable = true;
      pkcs11.enable = true;
      tctiEnvironment.enable = true;
    };
  };

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = true; # XXX

    users = {
      root.hashedPassword = null;

      arash = {
        isNormalUser = true;
        hashedPasswordFile = config.age.secrets.arash-pw.path;
        extraGroups = [ "wheel" ];
      };

      "${user.login}" = {
        isNormalUser = true;
        hashedPasswordFile = config.age.secrets."${user.login}-pw".path;
        extraGroups = [
          "adbusers"
          "docker"
          "i2c"
          "libvirtd"
          "lp"
          "plugdev"
          "scanner"
          "wheel"
        ];
      };
    };
  };

  home-manager = {
    users."${user.login}" = import "${root}/home";
    extraSpecialArgs = {
      inherit inputs root user;
      stateVersion = config.system.stateVersion;
    };
  };

  environment = {
    defaultPackages = lib.mkForce [ ];

    systemPackages = (with pkgs; [
      lshw
      smartmontools
      usbutils

      git
      rsync

      (hunspellWithDicts [ hunspellDicts.en_US hunspellDicts.en_US-large ])
    ])
    ++
    (with pkgs.pkgsMusl; [
      hdparm
      lm_sensors
      pciutils
    ]);
  };

  programs = {
    adb.enable = true;
    dconf.enable = true;
    hyprland.enable = true;
    light.enable = true;
    mosh.enable = true;
    mtr.enable = true;
    nix-ld.enable = true;
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

  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];

  documentation = {
    dev.enable = true;
    man.generateCaches = true;
  };

  age = {
    secrets = {
      "${user.login}-pw".file = "${root}/secrets/${user.login}-pw.age";
      "arash-pw".file = "${root}/secrets/arash-pw.age";
    };

    identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  };

  zramSwap.enable = true;

  system.stateVersion = lib.mkForce "24.11";

  networking.useDHCP = lib.mkDefault true;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
