{ config, pkgs, lib, modulesPath, domain, ... }:

let
  esc-nl = str: builtins.replaceStrings [ "\n" ] [ "\\n" ] str;
  rev-ip4 = with lib; ip4: "${concatStringsSep "." (reverseList (splitString "." ip4))}.in-addr.arpa";

  subnet = "192.168.0";
  subnet-rev-domain = rev-ip4 subnet;

  hostname = "hydrogen";
  ip = "${subnet}.1";

  wan = "enp1s0f0";
  lan = "enp1s0f1";

  zone-lanthanum = ''
    $ORIGIN ${domain}.
    $TTL 5m
    @ SOA ns1 hostmaster (
      2   ; serial
      4h  ; refresh
      15m ; retry
      8h  ; expire
      4m  ; neg cache TTL
    )
    @ NS ns1
    @ A ${ip} 

    ns1 A ${ip}
    ${hostname} A ${ip}
  '';

  zone-192-168-0 = ''
    $ORIGIN ${subnet-rev-domain}.
    $TTL 5m
    @ SOA ns1.${domain}. hostmaster.${domain}. (
      2   ; serial
      4h  ; refresh
      15m ; retry
      8h  ; expire
      4m  ; neg cache TTL
    )
    @ NS ns1.${domain}.

    ${rev-ip4 ip}. PTR ${hostname}.${domain}.
  '';

  seafile-docker-compose = ''
    version: '3'
    
    services:
      db:
        image: "mariadb:10.5"
        container_name: seafile-mysql
        environment:
          - MYSQL_ROOT_PASSWORD=gRPyfrdRhp8fPrEKKuc9
          - MYSQL_LOG_CONSOLE=true
        volumes:
          - /tank/seafile/db:/var/lib/mysql
    
      memcached:
        image: "memcached:alpine"
        container_name: seafile-memcached
    
      seafile:
        image: "seafileltd/seafile-mc:8.0.7"
        container_name: seafile
        ports:
          - "8080:80"
        volumes:
          - /tank/seafile/data:/shared
        environment:
          - DB_HOST=db
          - DB_ROOT_PASSWD=gRPyfrdRhp8fPrEKKuc9
          - TIME_ZONE=America/Chicago
          - SEAFILE_ADMIN_EMAIL=armeen@armeen.org
          - SEAFILE_ADMIN_PASSWORD=asecret
          - SEAFILE_SERVER_LETSENCRYPT=false
          - SEAFILE_SERVER_HOSTNAME=127.0.0.1
        depends_on:
          - db
          - memcached
  '';

  seafile-mysql-root-pw = "gRPyfrdRhp8fPrEKKuc9";
  seafile-extra-opts = [
    "--pod=seafile-pod"
    "--add-host=db:127.0.0.1"
    "--add-host=seafile-mysql:127.0.0.1"
    "--add-host=memcached:127.0.0.1"
    "--add-host=seafile-memcached:127.0.0.1"
    "--add-host=seafile:127.0.0.1"
  ];
in
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ./nftables.nix
  ];

  disabledModules = [ "services/networking/nftables.nix" ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/e07a94c0-425f-4ee5-ba23-7a209120d704";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/22B4-DF31";
      fsType = "vfat";
    };

    "/tank" = {
      device = "tank";
      fsType = "zfs";
    };

    "/tank/seafile" = {
      device = "tank/seafile";
      fsType = "zfs";
    };

    "/tank/arash" = {
      device = "tank/arash";
      fsType = "zfs";
    };
  };

  boot = {
    initrd.availableKernelModules = [ "ehci_pci" "ahci" "sd_mod" ];
    kernelModules = [ "kvm-intel" ];

    tmpOnTmpfs = true;

    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    initrd.supportedFilesystems = [ "zfs" ];
    supportedFilesystems = [ "zfs" ];

    kernel.sysctl = {
      "net.ipv4.conf.all.forwarding" = true;

      "net.ipv6.conf.all.forwarding" = true;
      "net.ipv6.conf.all.accept_ra" = 0;
      "net.ipv6.conf.all.autoconf" = 0;
      "net.ipv6.conf.all.use_tempaddr" = 0;

      "net.ipv6.conf.${wan}.accept_ra" = 2;
      "net.ipv6.conf.${wan}.autoconf" = 1;
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

  users = {
    defaultUserShell = pkgs.zsh;
    
    groups = {
      seafile = {};
    };

    users = {
      armeen = {
        isNormalUser = true;
        extraGroups = [ "wheel" "tank" "audio" ];
      };

      arash = {
        isNormalUser = true;
        extraGroups = [ "wheel" "tank" ];
      };

      seafile = {
        isSystemUser = true;
        group = "seafile";
        subUidRanges = [ { count = 65534; startUid = 100001; } ];
        subGidRanges = [ { count = 65534; startGid = 100001; } ];
      };

      mpd.extraGroups = [ "audio" ];
    };
  };

  environment = {
    defaultPackages = lib.mkForce [];

    systemPackages = with pkgs; [
      bottom
      conntrack-tools
      docker-compose
      efibootmgr
      file
      foot.terminfo
      fuse-overlayfs
      fzf
      git
      htop
      iperf
      killall
      ldns
      lsof
      mailutils
      mpc_cli
      ncmpcpp
      nmap
      ntfs3g
      openssl
      pamixer
      podman-compose
      ripgrep
      rng-tools
      seafile-shared
      smartmontools
      tcpdump
      tmux
      tree
      unrar
      unzip
      wget
      whois
    ];

    etc."podman/compose/seafile/docker-compose.yml".text = seafile-docker-compose;
  };

  # TODO: Switch to systemd-networkd
  networking = {
    hostName = hostname;
    hostId = "cc04ee17";
    domain = domain;

    nameservers = [ "127.0.0.1" ];

    interfaces = {
      enp3s0.useDHCP = true;
      "${wan}".useDHCP = true;

      lo = {
        ipv4.addresses = [
          { address = "127.0.0.1"; prefixLength = 8; }
          { address = "127.0.0.2"; prefixLength = 8; }
        ];
      };

      "${lan}" = {
        useDHCP = false;
        ipv4.addresses = [{
          address = ip;
          prefixLength = 24;
        }];
      };
    };

    # Handled by nftables
    nat.enable = false;
    firewall.enable = false;

    nftables = {
      enable = true;
      ruleset = ''
        table ip nft_filter {
          # Enable flow offloading for better throughput
          flowtable f {
            hook ingress priority 0;
            devices = { ${wan}, ${lan} };
          }

          chain output {
            type filter hook output priority 100; policy accept;
          }

          chain input {
            type filter hook input priority filter; policy drop;

            # Allow trusted networks to access the router
            iifname "${lan}" counter accept

            # Allow returning traffic from WAN and drop everthing else
            iifname "${wan}" ct state { established, related } counter accept
            #iifname "${wan}" tcp dport { ssh, http, https } counter accept
            iifname "${wan}" tcp dport { ssh } counter accept
            iifname "${wan}" drop

            iif lo accept
            ct state established,related accept comment "Accept traffic originated from us"
          }
          
          chain forward {
            type filter hook forward priority filter; policy drop;

            # Enable flow offloading for better throughput
            ip protocol { tcp, udp } flow offload @f

            # Allow trusted network WAN access
            iifname {
                    "${lan}",
            } oifname {
                    "${wan}",
            } counter accept comment "Allow trusted LAN to WAN"

            # Allow established WAN to return
            iifname {
                    "${wan}",
            } oifname {
                    "${lan}",
            } ct state established,related counter accept comment "Allow established back to LANs"
          }
        }

        table ip nft_nat {
          chain prerouting {
            type nat hook output priority filter; policy accept;
          }

          # Setup NAT masquerading on the WAN interface
          chain postrouting {
            type nat hook postrouting priority filter; policy accept;
            oifname "${wan}" masquerade
          } 
        }
      '';
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

  programs = {
    mtr.enable = true;
    zsh.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    neovim = {
      enable = true;
      defaultEditor = true;
      viAlias = true;
      vimAlias = true;
    };
  };

  services = {
    iperf3.enable = true;
    openssh.enable = true;

    bind = {
      enable = true;

      listenOn = [];
      listenOnIpv6 = [];
      forwarders = [];
      cacheNetworks = [ "127.0.0.0/8" ];

      zones = {
        "${domain}" = {
          master = true;
          file = "/var/lib/bind/db.${domain}";
          extraConfig = ''
          allow-update { key rndc-key; };
        '';
        };

        "${subnet-rev-domain}" = {
          master = true;
          file = "/var/lib/bind/db.192.168.0";
          extraConfig = ''
          allow-update { key rndc-key; };
        '';
        };
      };

      extraOptions = ''
      listen-on port 53 { 127.0.0.2; };
      recursion no;
      allow-transfer { none; };
      dnssec-enable yes;
      dnssec-validation yes;
      dnssec-lookaside auto;
      auth-nxdomain no;
    '';
    };

    dhcpd4 = {
      enable = true;
      interfaces = [ lan ];

      extraConfig = ''
          include "/etc/bind/rndc.key";

          option domain-name "${domain}";
          option domain-name-servers ${ip};
          
          ddns-update-style standard;
          ddns-rev-domainname "in-addr.arpa";
          deny client-updates;
          do-forward-updates on;
          update-optimization off;
          update-conflict-detection off;
          update-static-leases on;
          
          zone ${domain}. {
            primary 127.0.0.2;
            key rndc-key;
          }
          
          zone ${subnet-rev-domain}. {
            primary 127.0.0.2;
            key rndc-key;
          }
          
          subnet 192.168.0.0 netmask 255.255.255.0 {
            option broadcast-address 192.168.0.255;
            option routers ${ip};
            interface ${lan};
            range 192.168.0.128 192.168.0.254;
            default-lease-time 86400;
            max-lease-time 2592000;
          }
        '';
    };

    mpd = {
      enable = true;
      extraConfig = ''
          audio_output {
            type "pulse"
            name "Pulseaudio"
            server "127.0.0.1"
          }
        '';
    };

    nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedProxySettings = true;

      virtualHosts."nixpower.net" = {
        enableACME = true;
        forceSSL = true;
        root = "/var/www/nixpower.net";
      };

      virtualHosts."carbon.nixpower.net" = {
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://127.0.0.1:8080";
      };
    };

    restic = {
      backups.b2backup = {
        paths = [ "/tank/seafile" ];
        repository = "b2:Nixnet-Backup";
        passwordFile = config.sops.secrets.b2_pw.path;
        environmentFile = config.sops.secrets.b2_env.path;
      };
    };

    # TODO: Mail
    smartd = {
      enable = true;
    };

    unbound = {
      enable = true;
      resolveLocalQueries = false;
      localControlSocketPath = "/run/unbound/unbound.ctl";

      settings = {
        server = {
          hide-identity = true;
          hide-version = true;
          use-caps-for-id = true;
          do-not-query-localhost = false;
          so-reuseport = true;
          verbosity = 2;

          interface = [ ip "127.0.0.1" ];
          access-control = [ "127.0.0.0/8 allow" "192.168.0.0/24 allow" ];

          tls-cert-bundle = "/etc/ssl/certs/ca-certificates.crt";

          domain-insecure = [ domain subnet-rev-domain ];
          local-zone = [
            "${domain}.            nodefault"
            "168.192.in-addr.arpa. nodefault"
          ];
        };

        stub-zone = [
          { name = domain; stub-addr = "127.0.0.2"; }
          { name = subnet-rev-domain; stub-addr = "127.0.0.2"; }
        ];

        forward-zone = [{
          name = ".";
          forward-addr = [
            "9.9.9.9@853#dns.quad9.net"
            "149.112.112.112@853#dns.quad9.net"
            "1.1.1.1@853#cloudflare-dns.com"
            "1.0.0.1@853#cloudflare-dns.com"
          ];
          forward-tls-upstream = true;
        }];
      };
    };

    ympd = {
      enable = true;
      webPort = 8081;
    };

    zfs = {
      trim.enable = true;
      autoScrub.enable = true;
      autoSnapshot.enable = true;
    };
  };

  systemd = {
    tmpfiles.rules = [
      "d /var/lib/bind 0755 named nogroup"
      "f+ /var/lib/bind/db.${domain} 0644 named nogroup - ${esc-nl zone-lanthanum}"
      "f+ /var/lib/bind/db.192.168.0 0644 named nogroup - ${esc-nl zone-192-168-0}"
    ];

    targets.seafile = {
      description = "Seafile fileserver";
      wantedBy = [ "multi-user.target" ];
    };

    services = {
      mpd.serviceConfig.LimitMEMLOCK = "infinity";

      "podman-compose@" = {
        description = "%i service with Podman Compose";
        after = [ "network.target" ];
        wantedBy = [ "seafile.target" ];
        path = with pkgs; [ podman slirp4netns fuse-overlayfs ];

        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          WorkingDirectory = "/etc/podman/compose/%i";
          ExecStart = "${pkgs.podman-compose}/bin/podman-compose up -d --remove-orphans";
          ExecStop = "${pkgs.podman-compose}/bin/podman-compose down";
          #User = "seafile";
          #Group = "nogroup";
        };
      };
    };
  };

  security = {
    rtkit.enable = true;
    
    acme = {
      acceptTerms = true;
      email = "mahdianarmeen@gmail.com";
    };
  };

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    systemWide = true;

    tcp = {
      enable = true;
      anonymousClients.allowedIpRanges = [ "127.0.0.1" ];
    };
  };

  sops = {
    defaultSopsFile = ./secrets/secrets.yaml;
    age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

    secrets = {
      b2_pw = {};

      b2_env = {
        format = "binary";
        sopsFile = ./secrets/b2.env;
      };
    };
  };

  system.stateVersion = lib.mkForce "21.05";
}
