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
      '';
    };
  };
}
