{ config, pkgs, lib, modulesPath, inputs, root, user, domain, ... }:

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
      options = "noatime";
    };
  };

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
    kernelModules = [ "kvm-intel" ];

    binfmt.emulatedSystems = [ "aarch64-linux" ];

    loader = {
      efi.canTouchEfiVariables = true;

      systemd-boot = {
        enable = true;
        editor = true; # XXX
      };
    };
  };

  networking = {
    inherit domain;

    hostName = "argentum";

    wireless.iwd.enable = true;
  };

  hardware = {
    enableAllFirmware = true;

    bluetooth.enable = true;
    cpu.intel.updateMicrocode = true;
    rtl-sdr.enable = true;
    sensor.iio.enable = true;
    video.hidpi.enable = true;

    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };

    sane = {
      enable = true;
      extraBackends = with pkgs; [
        sane-airscan
      ];
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Chicago";

  console = {
    keyMap = "us";
    font = "Tamsyn7x13r";
    packages = [ pkgs.tamsyn ];
    earlySetup = false;
  };

  nix = {
    package = pkgs.nixUnstable;
    nixPath = lib.mkForce [ "nixpkgs=${config.nix.registry.nixpkgs.flake}" ];

    registry = {
      nixpkgs.flake = inputs.unstable;
      ns.flake = inputs.stable;
      nu.flake = inputs.unstable;
      nur.flake = inputs.nur;
    };

    settings = {
      allowed-users = lib.mkForce [ "@wheel" "sam" ];
      trusted-users = lib.mkForce [ "@wheel" "sam" ];

      substituters = [
        "https://cache.ngi0.nixos.org"
        "https://nix-community.cachix.org"
      ];

      trusted-public-keys = [
        "cache.ngi0.nixos.org-1:KqH5CBLNSyX184S9BKZJo1LxrxJ9ltnY2uAs5c/f1MA="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };

    extraOptions = ''
      warn-dirty = false
      experimental-features = flakes nix-command ca-derivations
    '';
  };

  nixpkgs = {
    hostPlatform = "x86_64-linux";
  };

  services = {
    blueman.enable = true;
    flatpak.enable = true;
    fstrim.enable = true;
    haveged.enable = true;
    physlock.enable = true;
    saned.enable = true;
    smartd.enable = true;
    tcsd.enable = false;
    timesyncd.enable = true;
    udisks2.enable = true;
    fwupd.enable = true;

    avahi = {
      enable = true;
      nssmdns = true;
    };

    hardware = {
      bolt.enable = true;
    };

    openssh = {
      enable = true;
      forwardX11 = true;

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
      jack.enable = true;
    };

    printing = {
      enable = true;
      drivers = with pkgs; [
        gutenprint
        gutenprintBin
        cnijfilter2
      ];
    };

    udev = {
      packages = with pkgs; [
        ledger-udev-rules
        yubikey-personalization
      ];
    };

    usbguard = {
      enable = false;
      rules = builtins.readFile ./conf/usbguard/rules.conf;
    };

    xserver.videoDrivers = [ "intel" ];
  };

  systemd = {
    watchdog.rebootTime = "15s";

    tmpfiles.rules = [
      "d /run/cache 0755 - - -"
      "d /var/etc 0755 - - -"
      "d /var/srv 0755 - - -"
      "d /run/tmp 1777 - - -"

      "L /srv - - - - /var/srv"
      "L /tmp - - - - /run/tmp"

      # Using /home/root instead
      "R /root - - - - -"

      # For Wolfram kernel
      "L /bin/uname - - - - ${pkgs.coreutils}/bin/uname"
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

      loginLimits = [{
        domain = "*";
        type = "soft";
        item = "nofile";
        value = "65536";
      }];

      services.swaylock.fprintAuth = true;
      services.login.fprintAuth = true;
    };

    tpm2 = {
      enable = true;
      abrmd.enable = true;
      pkcs11.enable = true;
      tctiEnvironment.enable = true;
    };
  };

  virtualisation = {
    podman = {
      enable = true;
      defaultNetwork.settings.dns_enabled = true;
    };
  };

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = true; # XXX

    users = {
      root = {
        #hashedPassword = null;
        home = lib.mkForce "/home/root";
      };

      "${user.login}" = {
        isNormalUser = true;
        #passwordFile = config.sops.secrets."${user.login}-pw".path;
        extraGroups = [
          "adbusers"
          "docker"
          "i2c"
          "libvirtd"
          "lp"
          "plugdev"
          "scanner"
          "video"
          "wheel"
        ];
      };
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

      #(mathematica.override {
      #  version = "13.1.0";
      #  config.cudaSupport = true;
      #})

      (hunspellWithDicts [ hunspellDicts.en_US hunspellDicts.en_US-large ])

      #(lkrg.override { kernel = config.boot.kernelPackages.kernel; })
    ])
    ++
    (with pkgs.pkgsMusl; [
      hdparm
      lm_sensors
      pciutils
    ]);

    etc = {
      /*
      "ssh/ssh_host_ed25519_key".source = "/var/etc/ssh/ssh_host_ed25519_key";
      "ssh/ssh_host_ed25519_key.pub".source = "/var/etc/ssh/ssh_host_ed25519_key.pub";
      "ssh/ssh_host_rsa_key".source = "/var/etc/ssh/ssh_host_rsa_key";
      "ssh/ssh_host_rsa_key.pub".source = "/var/etc/ssh/ssh_host_rsa_key.pub";
      */

      openvpn.source = "${pkgs.update-resolv-conf}/libexec/openvpn";
    };
  };

  programs = {
    adb.enable = true;
    dconf.enable = true;
    hyprland.enable = true;
    light.enable = true;
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

  documentation = {
    dev.enable = true;
    man.generateCaches = true;
  };

/*
  sops = {
    defaultSopsFile = "${root}/secrets/secrets.yaml";
    age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

    secrets = {
      "${user.login}-pw".neededForUsers = true;
      "sam-pw".neededForUsers = true;
    };
  };
*/

  zramSwap.enable = true;

  system.stateVersion = lib.mkForce "23.05";

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp166s0.useDHCP = lib.mkDefault true;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
