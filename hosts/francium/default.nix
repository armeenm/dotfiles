inputs@{ config, pkgs, lib, modulesPath, user, domain, ... }:

let
  hostName = "francium";
  ip4 = "205.185.123.206";
  domain = inputs.domain;
in
{
  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

  boot = {
    initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci" "virtio_scsi" "sd_mod" "sr_mod" "virtio_blk" ];
    kernelModules = [ "kvm-amd" ];

    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/vda";
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/e7ea314b-eed4-4e5c-862d-044893fead95";
      fsType = "ext4";
    };

    "/tank" = {
      device = "/dev/disk/by-uuid/d7fa6066-d6ab-4203-babc-b73a21d35874";
      fsType = "ext4";
    };
  };

  nix.trustedUsers = [ "@wheel" ];

  networking = {
    inherit hostName domain;
    interfaces.ens3.useDHCP = true;

    firewall = {
      allowedTCPPorts = [ 22 80 443 5222 5269 5280 ];
    };
  };

  time.timeZone = "America/Denver";
  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    keyMap = "us";
    font = "Tamsyn7x13r";
    packages = [ pkgs.tamsyn ];
    earlySetup = true;
  };

  users = {
    defaultUserShell = pkgs.zsh;

    users = {
      root = {
        hashedPassword = null;
        home = lib.mkForce "/home/root";
      };

      "${user.login}" = {
        isNormalUser = true;
        extraGroups = [ "wheel" ];
      };

      nginx.extraGroups = [ "acme" ];
      prosody.extraGroups = [ "acme" ];
    };
  };

  environment = {
    defaultPackages = lib.mkForce [];
    systemPackages = with pkgs; [
      bottom
      fd
      git
      ldns
      nmap
      ripgrep
      rxvt_unicode.terminfo
      tmux
      wget
      profanity
      tree
    ];

    shellAliases = {
      sudo = "doas";
    };
  };

  programs = {
    mosh.enable = true;
    mtr.enable = true;
    zsh.enable = true;

    neovim = {
      enable = true;
      defaultEditor = true;
      viAlias = true;
      vimAlias = true;
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

  services = {
    openssh.enable = true;

    prosody = {
      enable = true;
      admins = [ "${user.login}@${domain}" ];

      ssl = {
        cert = "/var/lib/acme/${domain}/fullchain.pem";
        key = "/var/lib/acme/${domain}/key.pem";
      };

      modules = {
        announce = true;
        websocket = true;
      };

      virtualHosts."${domain}" = {
        enabled = true;
        domain = "${domain}";

        ssl = {
          cert = "/var/lib/acme/${domain}/fullchain.pem";
          key = "/var/lib/acme/${domain}/key.pem";
        };
      };

      muc = [ {
        domain = "muc.krypton.${domain}";
      } ];

      uploadHttp = {
        domain = "upload.krypton.${domain}";
      };
    };

    nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;

      virtualHosts = {
        "acmechallenge.${domain}" = {
          serverAliases = [ "*.${domain}" ];

          locations = {
            "/".return = "301 https://$host$request_uri";
            "/.well-known/acme-challenge".root = "/var/lib/acme/.challenges";
          };
        };
      };

      #virtualHosts."${domain}" = {
      #  enableACME = true;

      #  listen = [ {
      #    addr = ip4;
      #    port = 18089;
      #    ssl = true;
      #  } ];

      #  locations."/".proxyPass = "http://[::1]:18089";
      #};

      #virtualHosts."${domain}" = {
      #  enableACME = true;

      #  listen = [ {
      #    addr = ip4;
      #    port = 5280;
      #    ssl = true;
      #  } ];

      #  serverAliases = [
      #    "muc.krypton.${domain}"
      #    "upload.krypton.${domain}"
      #  ];

      #  locations."/xmpp-websocket" = {
      #    proxyPass = "http://[::1]:5280/xmpp-websocket";
      #    extraConfig = ''
      #      proxy_http_version 1.1;
      #      proxy_set_header Upgrade $http_upgrade;
      #      proxy_set_header Connection "Upgrade";

      #      proxy_set_header Host $host;
      #      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      #      proxy_set_header X-Forwarded-Proto $scheme;
      #      proxy_read_timeout 900s;
      #    '';
      #  };
      #};
    };

    monero = {
      enable = false;
      dataDir = "/tank/monero";

      rpc = {
        address = "127.0.0.1";
        port = 18081;
      };

      extraConfig = ''
        rpc-restricted-bind-port=18089
        enforce-dns-checkpointing=1
        enable-dns-blocklist=1
        no-igd=1
        no-zmq=1
        public-node=1
      '';
    };
  };

  security = {
    auditd.enable = true;
    sudo.wheelNeedsPassword = false;

    protectKernelImage = true;
    unprivilegedUsernsClone = false;
    allowUserNamespaces = true;

    acme = {
      acceptTerms = true;
      email = user.email;

      certs = {
        "${domain}" = {
          email = user.email;
          webroot = "/var/lib/acme/.challenges";
          extraDomainNames = [
            "xmpp.krypton.${domain}"
            "upload.krypton.${domain}"
          ];
        };
      };
    };

    doas = {
      enable = true;
      extraRules = [{
        groups = [ "wheel" ];
        keepEnv = true;
        noPass = true;
      }];
    };
  };

  system.stateVersion = lib.mkForce "21.05";
}

