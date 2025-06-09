{ config, pkgs, lib, user, ... }:

{
  boot = {
    supportedFilesystems = [ "nfs" "nfs4" ];
    loader.systemd-boot.enable = lib.mkForce false; # NOTE: Conflicts with lanzaboote.

    initrd = {
      kernelModules = [ "amdgpu" ];
      includeDefaultModules = false;
    };

    lanzaboote = {
      enable = true;
      pkiBundle = "/etc/secureboot";
    };

    kernelModules = [
      # Infiniband
      "ib_umad"
      "ib_ipoib"
    ];

    kernelPackages = pkgs.callPackage ./kernel.nix { };

    kernelParams = [
      "elevator=none"
      "kvm.nx_huge_pages=force"
      "lsm=yama,apparmor,bpf"
      "quiet"
      "slub_debug=FZ"
      "udev.log_priority=3"
    ];

    kernel.sysctl = {
      # Needed for router.
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
  };

  environment = {
    systemPackages = with pkgs; [
      opensm
      radeontop
      rdma-core
    ];
  };

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/99E6-DA3E";
      fsType = "vfat";
    };

    "/" = {
      device = "/dev/disk/by-uuid/9d5b84ba-b19b-46ef-9514-d275cf305ddb";
      fsType = "ext4";
    };
  };

  hardware = {
    bluetooth.enable = true;
    cpu.amd.updateMicrocode = true;
    enableAllFirmware = true;
    nvidia-container-toolkit.enable = true;
    nvidia.open = false;
    rtl-sdr.enable = true;

    graphics = {
      extraPackages = with pkgs; [
        amdvlk
        rocmPackages.clr.icd
      ];
      extraPackages32 = with pkgs; [ driversi686Linux.amdvlk ];
    };

    nvidia = {
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      modesetting.enable = true;
    };
  };

  home-manager.users."${user.login}" = {
    services.shikane = {
      enable = true;
      settings = {
        profile = [
          {
            name = "default";
            output = [
              {
                enable = true;
                search = [ "m=XG270QG" "s=#ASN3IWn/L/jd" "v=ViewSonic Corporation" ];
                mode = "best";
                position = {
                  x = 0;
                  y = 525;
                };
              }
              {
                enable = true;
                search = [ "m=LG Ultra HD" "s=0x00028C0E" "v=LG Electronics" ];
                mode = "best";
                position = {
                  x = 2560;
                  y = 0;
                };
              }
            ];
          }
        ];
      };
    };
  };

  networking = {
    domain = "armeen.xyz";
    hostId = "5a656e88";
    hostName = "lithium";

    firewall.interfaces.enp78s0.allowedTCPPorts = [ 8080 8888 7860 5201 11434 ];
  };

  nixpkgs = {
    hostPlatform = "x86_64-linux";
    config = {
      cudaSupport = false;
      rocmSupport = true;
    };
  };

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
    i2pd.enable = true;
    iperf3.enable = true;
    xserver.videoDrivers = [ "amdgpu" "nvidia" ];

    hardware = {
      bolt.enable = true;
      openrgb = {
        enable = true;
        package = pkgs.openrgb-with-all-plugins;
        motherboard = "amd";
      };
    };

    ollama = {
      enable = true;
      host = "0.0.0.0";
      rocmOverrideGfx = "10.3.0";
    };

    sunshine = {
      enable = true;
      capSysAdmin = true;
      openFirewall = true;
    };

    tor = {
      enable = true;
      client.enable = true;
    };
  };

  system.stateVersion = lib.mkForce "24.11";

  systemd = {
    mounts = [
      {
        type = "nfs";
        what = "carbon.armeen.xyz:/export/tank";
        where = "/mnt/tank";
        options = "sec=krb5p";
      }
    ];

    automounts = [
      {
        wantedBy = [ "multi-user.target" ];
        automountConfig = {
          TimeoutIdleSec = "120";
        };
        where = "/mnt/tank";
      }
    ];
  };

  users.users."${user.login}".extraGroups = [
    "adbusers"
    "i2c"
    "input"
    "lp" # Printing
    "networkmanager"
    "plugdev"
    "scanner"
  ];

  virtualisation = {
    podman = {
      enable = true;
      defaultNetwork.settings.dns_enabled = true;
    };
  };
}
