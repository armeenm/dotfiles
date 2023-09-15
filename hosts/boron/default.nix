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
    kernelModules = [ "kvm-intel" ];

    initrd = {
      availableKernelModules = [ "xhci_pci" "usb_storage" "sd_mod" "sdhci_pci" ];

      systemd = {
        enable = true;
        network.enable = true;
      };

      verbose = false;
    };

    consoleLogLevel = 0;

    kernelParams = [
      "elevator=none"
      "kvm.nx_huge_pages=force"
      "lsm=yama,apparmor,bpf"
      "quiet"
      "slub_debug=FZ"
      "udev.log_priority=3"
    ];

    kernel.sysctl = {
      # Needed for router
      "net.ipv4.conf.all.accept_redirects" = true;
      "net.ipv6.conf.all.accept_redirects" = true;
      "net.ipv4.conf.all.accept_source_route" = true;
      "net.ipv6.conf.all.accept_source_route" = true;
      "net.ipv4.ip_forward" = true;
      "net.ipv4.conf.all.send_redirects" = true;

      "net.ipv4.conf.all.secure_redirects" = true;
      "net.ipv6.conf.all.secure_redirects" = true;

      "net.ipv4.conf.all.log_martians" = true;
      "net.ipv4.conf.all.rp_filter" = true;

      "net.ipv4.icmp_echo_ignore_all" = false;
      "net.ipv4.icmp_echo_ignore_broadcasts" = true;
      "net.ipv4.icmp_ignore_bogus_error_responses" = true;

      "net.ipv4.tcp_congestion_control" = "bbr";
      "net.ipv4.tcp_dsack" = false;
      "net.ipv4.tcp_fack" = false;
      "net.ipv4.tcp_fastopen" = 3;
      "net.ipv4.tcp_rfc1337" = true;
      "net.ipv4.tcp_sack" = false;
      "net.ipv4.tcp_synack_retries" = 5;
      "net.ipv4.tcp_timestamps" = false;
      "net.ipv4.tcp_window_scaling" = true;

      "net.ipv6.conf.default.accept_ra" = false;
      "net.ipv6.conf.default.accept_ra_pinfo" = false;
      "net.ipv6.conf.default.accept_ra_rtr_pref" = false;
      "net.ipv6.conf.default.aceept_ra_defrtr" = false;
      "net.ipv6.conf.default.max_addresses" = 1;
      "net.ipv6.conf.default.router_solicitations" = false;

      "net.core.bpf_jit_harden" = 2;
      "net.core.default_qdisc" = "cake";
      "net.core.netdev_max_backlog" = 5000;
      "net.core.rmem_max" = 8388608;
      "net.core.wmem_max" = 8388608;

      "kernel.core_uses_pid" = true;
      "kernel.kptr_restrict" = 2;
      "kernel.panic_on_oops" = false;
      "kernel.perf_event_paranoid" = 3;
      "kernel.printk" = "3 3 3 3";
      "kernel.randomize_va_space" = 2;
      "kernel.unprivileged_bpf_disabled" = true;
      "kernel.yama.ptrace_scope" = 2;

      # Appropriate for x86.
      "vm.max_map_count" = 1048576;
      "vm.mmap_rnd_bits" = 32;
      "vm.mmap_rnd_compat_bits" = 16;

      "user.max_user_namespaces" = 10000;

      "fs.protected_hardlinks" = true;
      "fs.protected_symlinks" = true;
      "fs.protected_fifos" = 2;
      "fs.protected_regular" = 2;
    };

    loader = {
      efi.canTouchEfiVariables = true;

      systemd-boot = {
        enable = true;
        editor = true; # XXX
      };
    };
  };

  networking = {
    hostId = "6f076f06";
    hostName = "boron";

    useNetworkd = true;
    wireless.iwd.enable = true;
  };

  hardware = {
    enableAllFirmware = true;

    bluetooth.enable = true;
    cpu.intel.updateMicrocode = true;

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
  time.timeZone = "America/Los_Angeles";

  console = {
    keyMap = "us";
    font = "Tamsyn7x13r";
    packages = [ pkgs.tamsyn ];
    earlySetup = false;
  };

  nix = {
    package = pkgs.nixUnstable;
    channel.enable = true; # NOTE: We should try to get rid of this.
    nixPath = lib.mkForce [ "nixpkgs=${config.nix.registry.nixpkgs.flake}" ];

    registry = {
      nixpkgs.flake = inputs.nixpkgs;
    };

    settings = {
      allowed-users = lib.mkForce [ "@wheel" ];
      trusted-users = lib.mkForce [ "@wheel" ];
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
    fstrim.enable = true;
    haveged.enable = true;
    iperf3.enable = true;
    physlock.enable = true;
    timesyncd.enable = true;
    udisks2.enable = true;
    fwupd.enable = true;

    avahi = {
      enable = true;
      nssmdns = true;
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
      jack.enable = true;
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

    resolved = {
      enable = true;
      fallbackDns = lib.mkForce [];
      dnssec = "false";
    };
  };

  systemd = {
    watchdog.rebootTime = "15s";
    services.systemd-networkd-wait-online.enable = lib.mkForce false;
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
      services.swaylock = {};
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

  environment = {
    defaultPackages = lib.mkForce [ ];

    systemPackages = (with pkgs; [
      efibootmgr
      hdparm
      lm_sensors
      lshw
      pciutils
      usbutils

      git
      rsync

      (hunspellWithDicts [ hunspellDicts.en_US hunspellDicts.en_US-large ])
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

  documentation = {
    dev.enable = true;
    man.generateCaches = true;
  };

  zramSwap.enable = true;

  system = {
    stateVersion = lib.mkForce "23.05";

    activationScripts.report-changes = ''
        PATH=$PATH:${lib.makeBinPath [ pkgs.nvd pkgs.nix ]}
        nvd diff $(ls -dv /nix/var/nix/profiles/system-*-link | tail -2)
    '';
  };
}
