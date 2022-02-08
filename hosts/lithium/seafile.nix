{ config, pkgs, domain, ... }:

let
  seafileDomain = "carbon.${domain}";
in {
  services = {
    #nginx.virtualHosts.${seafileDomain} = {
    #  enableACME = true;
    #  forceSSL = true;
    #  locations."/".proxyPass = "http://unix:/run/seahub/gunicorn.sock:";
    #};
    
    seafile = {
      enable = true;
      adminEmail = "armeen@armeen.org";
      initialAdminPassword = "temppwd!!!";
      ccnetSettings.General.SERVICE_URL = "https://${seafileDomain}";
    };
  };
}
