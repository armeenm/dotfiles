
inputs@{ config, pkgs, lib, modulesPath, root, user, domain, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ./home
  ];

  fileSystems = {
    "/" = {
      device = "rpool/root/nixos";
      fsType = "zfs";
    };
    "/home" = {
      device = "rpool/root/home";
      fsType = "zfs";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/4CDA-FEA2";
      fsType = "vfat";
    };
  };

  boot = {
    initrd = {
      includeDefaultModules = false;
      verbose = false;
      availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
    };
    
    supportedFilesystems = [ "zfs" ];

    kernelPackages = pkgs.callPackage ./kernel.nix {};

    consoleLogLevel = 0;

    kernelParams = [
      "kvm.nx_huge_pages=force"
      "lsm=yama,apparmor,bpf"
      "quiet"
      "slub_debug=FZ"
      "udev.log_priority=3"
    ];

    kernel = {
      randstructSeed = "aaaaa";

      sysctl = {
        "net.ipv4.conf.all.accept_redirects" = false;
        "net.ipv4.conf.all.accept_source_route" = false;
        "net.ipv4.conf.all.log_martians" = true;
        "net.ipv4.conf.all.rp_filter" = true;
        "net.ipv4.conf.all.secure_redirects" = false;
        "net.ipv4.conf.all.send_redirects" = false;
        "net.ipv4.conf.default.accept_redirects" = false;
        "net.ipv4.conf.default.accept_source_route" = false;
        "net.ipv4.conf.default.log_martians" = true;
        "net.ipv4.conf.default.rp_filter" = true;
        "net.ipv4.conf.default.secure_redirects" = false;
        "net.ipv4.icmp_echo_ignore_all" = true;
        "net.ipv4.icmp_echo_ignore_broadcasts" = true;
        "net.ipv4.icmp_ignore_bogus_error_responses" = true;
        "net.ipv4.ip_forward" = false;
        "net.ipv4.tcp_congestion_control" = "bbr";
        "net.ipv4.tcp_dsack" = false;
        "net.ipv4.tcp_fack" = false;
        "net.ipv4.tcp_fastopen" = 3;
        "net.ipv4.tcp_rfc1337" = true;
        "net.ipv4.tcp_sack" = false;
        "net.ipv4.tcp_synack_retries" = 5;
        "net.ipv4.tcp_timestamps" = false;
        "net.ipv4.tcp_window_scaling" = true;

        "net.ipv6.conf.all.accept_redirects" = false;
        "net.ipv6.conf.all.accept_source_route" = false;
        "net.ipv6.conf.all.secure_redirects" = false;
        "net.ipv6.conf.default.accept_ra" = false;
        "net.ipv6.conf.default.accept_ra_pinfo" = false;
        "net.ipv6.conf.default.accept_ra_rtr_pref" = false;
        "net.ipv6.conf.default.accept_redirects" = false;
        "net.ipv6.conf.default.aceept_ra_defrtr" = false;
        "net.ipv6.conf.default.max_addresses" = 1;
        "net.ipv6.conf.default.router_solicitations" = false;
        "net.ipv6.conf.default.secure_redirects" = false;

        "net.core.bpf_jit_harden" = 2;
        "net.core.default_qdisc" = "cake";
        "net.core.netdev_max_backlog" = 5000;
        "net.core.rmem_max" = 8388608;
        "net.core.wmem_max" = 8388608;

        "kernel.core_uses_pid" = true;
        "kernel.kptr_restrict" = 2;
        "kernel.panic_on_oops" = true;
        "kernel.perf_event_paranoid" = 3;
        "kernel.printk" = "3 3 3 3";
        "kernel.randomize_va_space" = 2;
        "kernel.unprivileged_bpf_disabled" = true;
        "kernel.yama.ptrace_scope" = 2;

        # Appropriate for x86
        "vm.max_map_count" = 1048576;
        "vm.mmap_rnd_bits" = 32;
        "vm.mmap_rnd_compat_bits" = 16;

        "user.max_user_namespaces" = 10000;

        "fs.protected_fifos" = 2;
        "fs.protected_hardlinks" = true;
        "fs.protected_regular" = 2;
        "fs.protected_symlinks" = true;
      };
    };

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot = {
        enable = true;
        editor = false;
      };
    };
  };

  nix = {
    package = pkgs.nixUnstable;
    allowedUsers = lib.mkForce [ "@wheel" "arash" ];
    extraOptions = ''
      warn-dirty = false
      experimental-features = flakes nix-command ca-derivations
    '';

    binaryCaches = [
      "https://cache.ngi0.nixos.org"
      "https://nix-community.cachix.org"
    ];

    binaryCachePublicKeys = [
      "cache.ngi0.nixos.org-1:KqH5CBLNSyX184S9BKZJo1LxrxJ9ltnY2uAs5c/f1MA="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  networking = {
    inherit domain;
    hostName = "lithium";
    hostId = "5a656e88";

    networkmanager.enable = true;
    wireguard.enable = true;

    firewall.checkReversePath = "loose";
  };

  time.timeZone = "America/Chicago";
  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    keyMap = "us";
    font = "Tamsyn7x13r";
    packages = [ pkgs.tamsyn ];
    earlySetup = false;
  };

  security = {
    auditd.enable = true;
    rtkit.enable = true;
    sudo.enable = false;

    protectKernelImage = true;
    unprivilegedUsernsClone = true;
    forcePageTableIsolation = false;
    allowUserNamespaces = true;

    virtualisation.flushL1DataCache = null;

    apparmor = {
      enable = true;
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
      
      loginLimits = [ {
        domain = "*";
        type = "soft";
        item = "nofile";
        value = "65536";
      } ];
    };

    tpm2 = {
      enable = true;
      abrmd.enable = true;
      pkcs11.enable = true;
      tctiEnvironment.enable = true;
    };
  };

  virtualisation = {
    spiceUSBRedirection.enable = true;
    waydroid.enable = true;
    
    libvirtd = {
      enable = true;
      qemu = {
        swtpm.enable = true;
        ovmf = {
          enable = true;
          package = (pkgs.OVMF.override {
            secureBoot = true;
            tpmSupport = true;
          });
        };
      };
    };
  };

  zramSwap.enable = true;

  services = {
    blueman.enable = true;
    fstrim.enable = true;
    haveged.enable = true;
    mullvad-vpn.enable = false;
    nix-serve.enable = true;
    pcscd.enable = true;
    physlock.enable = true;
    saned.enable = true;
    smartd.enable = true;
    spice-vdagentd.enable = true;
    tcsd.enable = false;
    timesyncd.enable = true;
    udisks2.enable = true;

    resolved = {
      enable = true;
      dnssec = "false";
      fallbackDns = [ "" ];
    };

    zfs = {
      trim.enable = true;
      autoScrub.enable = true;
      autoSnapshot.enable = true;
    };

    avahi = {
      enable = true;
      nssmdns = true;
    };

    chrony = {
      enable = false;
      enableNTS = true;
      servers = [
        "time.cloudflare.com"
      ];
    };

    openssh = {
      enable = true;
      forwardX11 = true;
    };

    usbguard = {
      enable = false;
      rules = builtins.readFile ./conf/usbguard/rules.conf;
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
        platformio
        yubikey-personalization
      ];

      extraRules = ''
        ACTION=="add|change"                                                    \
        , KERNEL=="sd[a-z]*[0-9]*|mmcblk[0-9]*p[0-9]*|nvme[0-9]*n[0-9]*p[0-9]*" \
        , ENV{ID_FS_TYPE}=="zfs_member"                                         \
        , ATTR{../queue/scheduler}="none"
      '';
    };

    tor = {
      enable = true;
      client.enable = true;
    };

    xserver.videoDrivers = [ "nvidia" ];
  };

  hardware = {
    bluetooth.enable = true;
    cpu.amd.updateMicrocode = true;
    opengl.enable = true;
    rtl-sdr.enable = true;
    video.hidpi.enable = true;

    nvidia = {
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      modesetting.enable = true;
      #powerManagement.enable = true;
    };

    sane = {
      enable = true;
      extraBackends = with pkgs; [
        sane-airscan
      ];
    };
  };

  users = {
    mutableUsers = false;
    defaultUserShell = pkgs.zsh;

    users = {
      root = {
        hashedPassword = null;
        home = lib.mkForce "/home/root";
      };

      "${user.login}" = {
        isNormalUser = true;
        passwordFile = config.sops.secrets.armeen-pw.path;
        extraGroups = [
          "adbusers"
          "i2c"
          "libvirtd"
          "lp"
          "networkmanager"
          "plugdev"
          "scanner"
          "wheel"
        ];
      };

      arash = {
        isNormalUser = true;
        passwordFile = config.sops.secrets.arash-pw.path;
      };
    };
  };

  environment = {
    # TODO: Not really a good idea; find a better solution
    #memoryAllocator.provider = "graphene-hardened";

    defaultPackages = lib.mkForce [];
    systemPackages = with pkgs; [
      git
      rsync
      (mathematica.override {
        cudaSupport = true;
        nvidia_x11 = config.boot.kernelPackages.nvidiaPackages.stable;
        cudatoolkit = cudaPackages.cudatoolkit_11_5;
      })
    ];

    etc = {
      adjtime.source = "/var/etc/adjtime";
      NIXOS.source = "/var/etc/NIXOS";
      "zfs/zpool.cache".source = "/run/zpool.cache";
      "NetworkManager/system-connections".source = "/var/etc/NetworkManager/system-connections";

      "ssh/ssh_host_ed25519_key".source = "/var/etc/ssh/ssh_host_ed25519_key";
      "ssh/ssh_host_ed25519_key.pub".source = "/var/etc/ssh/ssh_host_ed25519_key.pub";
      "ssh/ssh_host_rsa_key".source = "/var/etc/ssh/ssh_host_rsa_key";
      "ssh/ssh_host_rsa_key.pub".source = "/var/etc/ssh/ssh_host_rsa_key.pub";
    };
  };

  systemd = {
    tmpfiles.rules = [
      "d /var/srv 0755 - - -"
      "d /var/etc 0755 - - -"
      "d /run/cache 0755 - - -"
      "d /run/tmp 1777 - - -"

      "L /srv - - - - /var/srv"
      "L /tmp - - - - /run/tmp"
      "R /root - - - - -"
      "L /bin/uname - - - - ${pkgs.coreutils}/bin/uname"

      "w- /sys/module/hid_apple/parameters/fnmode - - - - 1"
    ];

    suppressedSystemUnits = [
      "sys-kernel-debug.mount"
    ];

    watchdog.rebootTime = "30s";
  };

  programs = {
    adb.enable = true;
    dconf.enable = true;
    zsh.enable = true;

    custom.ddcutil = {
      enable = true;
      users = [ user.login ];
    };

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

  sops = {
    defaultSopsFile = ./secrets/secrets.yaml;
    age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

    secrets = {
      armeen-pw.neededForUsers = true;
      arash-pw.neededForUsers = true;
    };
  };

  documentation = {
    dev.enable = true;
    man.generateCaches = true;
  };
  
  system.stateVersion = lib.mkForce "21.11";
}
