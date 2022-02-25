{ config, pkgs, domain, ... }:

let
  adminEmail = "armeen@armeen.org";
  initialAdminPassword = "temppwd!!!";
  siteName = "Carbon";

  seafileDomain = "carbon.${domain}";

  seahub = { socket = "unix:/run/seahub/gunicorn.sock:"; };
  seafile = rec {
    host = "127.0.0.1";
    port = 8082;
    socket = "${host}:${builtins.toString port}";
  };

in {
  environment.etc."fail2ban/filter.d/seafile-auth.conf".text = ''
    # Fail2Ban filter for seafile
    #
    
    [INCLUDES]
    
    # Read common prefixes. If any customizations available -- read them from
    # common.local
    before = common.conf
    
    [Definition]
    
    _daemon = seaf-server
    
    failregex = Login attempt limit reached.*, ip: <HOST>
    
    ignoreregex = 
  '';
  
  services = {
    nginx.virtualHosts.${seafileDomain} = {
      enableACME = true;
      forceSSL = true;

      locations = {
        "/" = {
          proxyPass = "http://${seahub.socket}";
          extraConfig = ''
            client_max_body_size 0;
            proxy_read_timeout 20m;
          '';
        };

        "/seafhttp" = {
          proxyPass = "http://${seafile.socket}";
          extraConfig = ''
            rewrite ^/seafhttp(.*)$ $1 break;
            client_max_body_size 0;
            proxy_connect_timeout 10h;
            proxy_read_timeout 10h;
            proxy_send_timeout 10h;
            send_timeout 10h;
          '';
        };
      };
    };

    restic.backups.b2backup = {
      paths = [ "/var/lib/private/seafile" ];
      #user = "seafile";
      repository = "b2:Nixnet-Backup";
      passwordFile = config.sops.secrets.restic-seafile-pw.path;
      environmentFile = config.sops.secrets.b2-env.path;
    };
    
    seafile = {
      enable = true;
      inherit adminEmail initialAdminPassword;

      seafileSettings = {
        fileserver.host = seafile.host;
        fileserver.port = seafile.port;
      };

      ccnetSettings = {
        General.SERVICE_URL = "https://${seafileDomain}";
      };

      seahubExtraConf = ''
        FILE_SERVER_ROOT = 'https://${seafileDomain}/seafhttp'
        ALLOWED_HOSTS = ['.${seafileDomain}']
        ENABLE_SETTINGS_VIA_WEB = False
        SITE_NAME = '${siteName}'
        SITE_TITLE = '${siteName}'
        TIME_ZONE = 'America/Chicago'
      '';
    };
  };

  sops.secrets = {
    # Restic backup encryption key
    restic-seafile-pw = {
      #group = "seafile";
    };

    # Backblaze B2 credentials
    b2-env = {
      #group = "seafile";
      format = "binary"; # TODO: Add .env support to sops-nix
      sopsFile = ./secrets/b2.env.bin;
    };
  };
}
