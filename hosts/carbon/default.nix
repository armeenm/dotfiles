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

    "/" = {
      device = "/dev/disk/by-uuid/580bb1f1-73ac-444a-b4ca-5e36b502cdb4";
      fsType = "ext4";
      options = ["noatime"];
    };
  };

  boot = {

    initrd.availableKernelModules = [
      "ahci"
      "xhci_pci"
      "ehci_pci"
      "usbhid"
      "usb_storage"
      "uas"
      "sd_mod"
      "sr_mod"
    ];

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
    smartd.enable = false; # TODO: Enable when this machine has actual drives.
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

    vaultwarden = {
      enable = true;
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

        "gpt.fulminous-hill.com" = {
          enableACME = true;
          forceSSL = true;
          locations."/".proxyPass = "http://192.168.0.128:9000";
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
    mutableUsers = true; # XXX

    users = {
      root.hashedPassword = null;

      "${user.login}" = {
        isNormalUser = true;
        #passwordFile = config.sops.secrets."${user.login}-pw".path;
        extraGroups = [ "wheel" ];
      };
    };
  };

  environment = {
    defaultPackages = lib.mkForce [ ];

    systemPackages = with pkgs; [
      btop
      conntrack-tools
      ethtool
      git
      hdparm
      ldns
      lm_sensors
      lshw
      pciutils
      rsync
      smartmontools
      tcpdump
      usbutils
    ];
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

/*
  sops = {
    defaultSopsFile = "${root}/secrets/secrets.yaml";
    age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

    secrets = {
      "${user.login}-pw".neededForUsers = true;
    };
  };
  */

  zramSwap.enable = true;
  system.stateVersion = lib.mkForce "23.05";
}
