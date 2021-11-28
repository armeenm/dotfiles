inputs@{ config, pkgs, lib, user, domain, ... }:

let
  hostName = "francium";
  domain = inputs.domain;
in
{
  imports = [ ./hw-generated.nix ];

  system.stateVersion = lib.mkForce "21.05";

  nix.trustedUsers = [ "@wheel" ];

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/vda";
  };

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
    ];

    shellAliases = {
      sudo = "doas";
    };
  };

  programs = {
    mtr.enable = true;
    zsh.enable = true;
    mosh.enable = true;

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
      allowRegistration = true;

      httpInterfaces = [ "::1" ];
      httpsInterfaces = [ ];

      modules = {
        announce = true;
        websocket = true;
      };

      virtualHosts."krypton.${domain}" = {
        enabled = true;
        domain = "krypton.${domain}";
      };

      muc = [ {
        domain = "muc.krypton.${domain}";
      } ];

      uploadHttp = {
        domain = "upload.krypton.${domain}";
      };

      extraConfig = ''
        consider_websocket_secure = true
      '';
    };

    nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;

      #virtualHosts."aurum.${domain}" = {
      #  enableACME = true;
      #  forceSSL = true;
      #  locations."/".proxyPass = "http://[::1]:18089";
      #};

      virtualHosts."krypton.${domain}" = {
        enableACME = true;
        forceSSL = true;

        serverAliases = [
          "muc.krypton.${domain}"
          "upload.krypton.${domain}"
        ];

        locations."/xmpp-websocket" = {
          proxyPass = "http://[::1]:5280/xmpp-websocket";
          extraConfig = ''
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection $connection_upgrade;

            proxy_set_header Host $host;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_read_timeout 900s;
          '';
        };
      };
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
    sudo.enable = true;
    sudo.wheelNeedsPassword = false;

    protectKernelImage = true;
    unprivilegedUsernsClone = false;
    allowUserNamespaces = true;

    acme = {
      acceptTerms = true;
      email = user.email;

      #certs = {
      #  "krypton.${domain}" = {
      #    email = user.email;
      #    webroot = "/var/lib/acme/krypton.${domain}";
      #    extraDomainNames = [ "muc.krypton.${domain}" "upload.krypton.${domain}" ];
      #  };
      #};
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
}

