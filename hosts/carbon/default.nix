{ config, pkgs, lib, inputs, user, ... }:

{
  imports = [ ./router.nix ];

  age = {
    secrets = {
      cloudflare-api-token.file = ../../secrets/cloudflare-api-token.env.age;

      restic-pw = {
        file = ../../secrets/restic-pw.age;
        owner = "restic";
        group = "restic";
      };

      restic-b2-env = {
        file =  ../../secrets/restic-b2-env.age;
        owner = "restic";
        group = "restic";
      };

      vaultwarden-env = {
        file = ../../secrets/vaultwarden-env.age;
        owner = "vaultwarden";
        group = "vaultwarden";
      };

      cobalt-config = {
        file = ../../secrets/cobalt.yaml.age;
        owner = "dufs";
        group = "dufs";
      };
    };
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_6_13;

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

    /*
    "/srv/tank" = {
      device = "/dev/nvme0n1:/dev/nvme1n1";
      fsType = "bcachefs";
    };
    */

    "/export/tank" = {
      device = "/srv/tank";
      options = [ "bind" ];
    };
  };

  environment.systemPackages = with pkgs; [
    bcachefs-tools
    conntrack-tools
  ];

  hardware = {
    enableAllFirmware = true;
    cpu.intel.updateMicrocode = true;
  };

  nixpkgs.hostPlatform = "x86_64-linux";

  security = {
    acme = {
      acceptTerms = true;
      defaults.email = user.email;
    };

    krb5 = {
      enable = true;
      settings = {
        libdefaults = {
          default_realm = "ARMEEN.XYZ";
        };

        domain_realm = {
          "armeen.xyz" = "ARMEEN.XYZ";
        };

        realms = {
          "ARMEEN.XYZ" = {
            admin_server = "cobalt.armeen.xyz";
            kdc = [
              "cobalt.armeen.xyz"
            ];
          };
        };
      };
    };
  };

  services = {
    jellyfin.enable = true;

    cloudflare-dyndns = {
      enable = true;
      domains = [ "armeen.xyz" ];
      apiTokenFile = config.age.secrets.cloudflare-api-token.path;
    };

    home-assistant = {
      enable = true;
      extraComponents = [
        "esphome"
        "met"
        "radio_browser"
      ];

      config = {
        default_config = {};
      };
    };

    kerberos_server = {
      enable = true;
      settings = {
        realms = {
          "ARMEEN.XYZ" = {
            acl = [
              { access = "all"; principal = "admin/admin"; }
            ];
          };
        };
      };
    };

    navidrome = {
      enable = true;
      settings = {
        Address = "[::1]";
        MusicFolder = "/srv/tank/armeen/music";
        EnableSharing = true;
      };
    };

    nfs.server = {
      enable = true;
      createMountPoints = true;
      exports = ''
        /export/tank *(rw,no_root_squash,fsid=0,sec=krb5p)
      '';
    };

    restic = {
      backups = {
        b2 = {
          paths = [ "/srv/tank" ];
          exclude = [ "/srv/tank/nobak" ];
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
      enableQuicBPF = true;
      enableReload = true;
      recommendedOptimisation = true;
      recommendedTlsSettings = true;
      recommendedZstdSettings = true;
      recommendedProxySettings = true;
      package = pkgs.nginxQuic;

      virtualHosts = {
        "\"\"" = {
          default = true;
          extraConfig = ''
            return 444;
          '';
        };

        vault = { globalRedirect = "vault.armeen.xyz"; };
        "vault.armeen.xyz" = {
          enableACME = true;
          forceSSL = true;
          locations."/".proxyPass = "http://[::1]:8000";
        };

        cobalt = { globalRedirect = "cobalt.armeen.xyz"; };
        "cobalt.armeen.xyz" = {
          enableACME = true;
          forceSSL = true;
          kTLS = true;
          quic = true;
          # TODO: Switch to UDS
          locations."/".proxyPass = "http://[::1]:8001";

          extraConfig = ''
            ignore_invalid_headers off;
            client_max_body_size 0;
            proxy_buffering off;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $remote_addr;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_connect_timeout 300;
            proxy_http_version 1.1;
            proxy_set_header Connection "";
            chunked_transfer_encoding off;

            quic_gso on;
            quic_retry on;
            add_header Alt-Svc 'h3=":443"; ma=86400';
          '';
        };

        music = { globalRedirect = "music.armeen.xyz"; };
        "music.armeen.xyz" = {
          enableACME = true;
          forceSSL = true;
          kTLS = true;
          quic = true;
          locations."/".proxyPass = "http://[::1]:4533";
          extraConfig = ''
            quic_gso on;
            quic_retry on;
            add_header Alt-Svc 'h3=":443"; ma=86400';
          '';
        };

        media = { globalRedirect = "media.armeen.xyz"; };
        "media.armeen.xyz" = {
          enableACME = true;
          forceSSL = true;
          kTLS = true;
          quic = true;

          locations = let
            proxyPass = "http://127.0.0.1:8096";
            commonProxy = ''
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
              proxy_set_header X-Forwarded-Protocol $scheme;
              proxy_set_header X-Forwarded-Host $http_host;
            '';
          in {
            "/" = {
              inherit proxyPass;
              extraConfig = commonProxy + ''
                # Disable buffering when the nginx proxy gets very resource heavy upon streaming.
                proxy_buffering off;
              '';
            };

            "/socket" = {
              inherit proxyPass;
              proxyWebsockets = true;
              extraConfig = commonProxy;
            };
          };

          extraConfig = builtins.readFile ./jellyfin_extra.txt;
        };
      };
    };

  };

  system.stateVersion = lib.mkForce "24.11";

  systemd = {
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

      cloudflare-dyndns.environment.HOME = "/tmp";
    };
  };

  users = {
    groups.restic = {};
    groups.dufs = {};

    users = {
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
}
