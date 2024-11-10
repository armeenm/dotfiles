{ config, pkgs, lib, modulesPath, inputs, root, user, ... }:

{
  imports = [
    ./router.nix
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/E777-4036";
      fsType = "vfat";
    };

    /*
    "/srv/tank" = {
      device = "/dev/nvme0n1:/dev/nvme1n1";
      fsType = "bcachefs";
    };
    */

    "/" = {
      device = "/dev/disk/by-uuid/580bb1f1-73ac-444a-b4ca-5e36b502cdb4";
      fsType = "ext4";
      options = ["noatime"];
    };
  };

  boot = {
    initrd = {
      supportedFilesystems = [ "bcachefs" ];

      availableKernelModules = [
        "ahci"
        "xhci_pci"
        "ehci_pci"
        "usbhid"
        "usb_storage"
        "uas"
        "sd_mod"
        "sr_mod"
      ];
    };

    kernelModules = [ "kvm-intel" ];

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

      # Appropriate for x86
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
        editor = false;
      };
    };
  };

  hardware = {
    enableAllFirmware = true;

    bluetooth.enable = true;
    cpu.intel.updateMicrocode = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  console = {
    keyMap = "us";
    font = "Tamsyn7x13r";
    packages = [ pkgs.tamsyn ];
    earlySetup = true;
  };

  nix = {
    nixPath = lib.mkForce [ "nixpkgs=${config.nix.registry.nixpkgs.flake}" ];

    registry = {
      nixpkgs.flake = inputs.nixpkgs;
    };

    settings = {
      allowed-users = lib.mkForce [ "@wheel" ];
      trusted-users = lib.mkForce [ "@wheel" ];

      experimental-features = [ "flakes" "nix-command" ];
      warn-dirty = false;
    };
  };

  nixpkgs = {
    hostPlatform = "x86_64-linux";
  };

  services = {
    fstrim.enable = true;
    fwupd.enable = true;
    haveged.enable = true;
    smartd.enable = true;
    tcsd.enable = false;
    timesyncd.enable = true;
    udisks2.enable = true;

    home-assistant = {
      enable = false;
      extraComponents = [
        "esphome"
        "met"
        "radio_browser"
      ];

      config = {
        default_config = {};
      };
    };

    openssh = {
      enable = true;

      settings = {
        PasswordAuthentication = false;
      };
    };

    restic = {
      backups = {
        b2 = {
          paths = [ "/srv/tank" ];
          user = "restic";
          repository = "b2:backups-jKl9AFet877bX";
          passwordFile = config.age.secrets.restic-pw.path;
          environmentFile = config.age.secrets.restic-b2-env.path;
        };
      };
    };

    vaultwarden = {
      enable = true;
      environmentFile = config.age.secrets.vaultwarden-env.path;
      backupDir = "/srv/tank/vaultwarden";

      config = {
        DOMAIN = "https://vault.armeen.xyz";
        SIGNUPS_ALLOWED = false;
        ROCKET_ADDRESS = "::1";
        ROCKET_PORT = "8000";
      };
    };

    nginx = {
      enable = true;

      virtualHosts = {
        "armeen.xyz" = {
          enableACME = true;
          forceSSL = true;
          root = "/srv/armeen-xyz";
        };

        "vault.armeen.xyz" = {
          enableACME = true;
          forceSSL = true;
          locations."/".proxyPass = "http://[::1]:8000";
          extraConfig = ''
            if ( $host != 'vault.armeen.xyz' ) {
              rewrite ^/(.*)$ https://vault.armeen.xyz/$1 permanent;
            }
          '';
        };

        "cobalt.armeen.xyz" = {
          enableACME = true;
          forceSSL = true;
          # TODO: Switch to UDS
          locations."/".proxyPass = "http://[::1]:8001";

          extraConfig = ''
            if ( $host != 'cobalt.armeen.xyz' ) {
              rewrite ^/(.*)$ https://cobalt.armeen.xyz/$1 permanent;
            }
          '';
        };
      };
    };

  };

  systemd = {
    watchdog.rebootTime = "15s";

    tmpfiles.rules = [
      "d /run/cache 0755 - - -"
      "d /var/etc 0755 - - -"
      "d /var/srv 0755 - - -"
      "d /run/tmp 1777 - - -"

      "L /srv - - - - /var/srv"
    ];

    services = {
      cobalt-dufs = {
        description = "Cobalt DUFS";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        serviceConfig = {
          Type = "exec";
          User = "dufs";
          ExecStart = ''
            ${pkgs.dufs}/bin/dufs -c ${config.age.secrets.cobalt-config.path}
          '';
        };
      };
    };
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
        noPass = true;
      }];
    };

    pam = {
      u2f.enable = true;
    };

    tpm2 = {
      enable = true;
      abrmd.enable = false;
      pkcs11.enable = true;
      tctiEnvironment.enable = true;
    };
  };

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;

    groups.restic = {};
    groups.dufs = {};

    users = {
      root.hashedPassword = null;

      "${user.login}" = {
        isNormalUser = true;
        hashedPasswordFile = config.age.secrets."${user.login}-pw".path;
        extraGroups = [ "wheel" ];
      };

      restic = {
        isSystemUser = true;
        group = "restic";
      };

      dufs = {
        isSystemUser = true;
        group = "dufs";
      };
    };
  };

  home-manager = {
    users."${user.login}" = import "${root}/home";
    extraSpecialArgs = {
      inherit inputs root user;
      stateVersion = config.system.stateVersion;
      isHeadless = true;
    };
  };

  environment = {
    defaultPackages = lib.mkForce [ ];

    systemPackages = with pkgs; [
      bcachefs-tools
      btop
      conntrack-tools
      doas-sudo-shim
      ethtool
      git
      hdparm
      keyutils
      ldns
      lm_sensors
      lshw
      parted
      pciutils
      rsync
      seaweedfs
      smartmontools
      tcpdump
      usbutils
      zellij
    ];

    etc = {
      swfs-filer = {
        source = ./filer.toml;
        target = "seaweedfs/filer.toml";
      };
    };
  };

  programs = {
    mtr.enable = true;
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

  age = {
    secrets = {
      "${user.login}-pw".file = "${root}/secrets/${user.login}-pw.age";

      restic-pw = {
        file = "${root}/secrets/restic-pw.age";
        owner = "restic";
        group = "restic";
      };

      restic-b2-env = {
        file = "${root}/secrets/restic-b2-env.age";
        owner = "restic";
        group = "restic";
      };

      vaultwarden-env = {
        file = "${root}/secrets/vaultwarden-env.age";
        owner = "vaultwarden";
        group = "vaultwarden";
      };

      cobalt-config = {
        file = "${root}/secrets/cobalt.yaml.age";
        owner = "dufs";
        group = "dufs";
      };
    };

    identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  };

  zramSwap.enable = true;
  system.stateVersion = lib.mkForce "24.11";
}
