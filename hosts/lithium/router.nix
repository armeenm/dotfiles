{ config, lib, domain, ... }:

let
  esc-nl = str: builtins.replaceStrings [ "\n" ] [ "\\n" ] str;
  rev-ip4 = with lib; ip4: "${concatStringsSep "." (reverseList (splitString "." ip4))}.in-addr.arpa";

  wan = "enp4s0f1";
  lan = "enp4s0f0";

  subnet = "192.168.0";
  subnetRevDomain = rev-ip4 subnet;

  hostname = "lithium";
  ip = "${subnet}.1";
  wanDomain = "lanthanum.${domain}";

  upstreamDns = [
    "2620:fe::11@853#dns.quad9.net"
    "2620:fe::fe:11@853#dns.quad9.net"
    "9.9.9.11@853#dns.quad9.net"
    "149.112.112.11@853#dns.quad9.net"
  ];

  soa = ''
    @ SOA ns1 hostmaster (
      2   ; serial
      4h  ; refresh
      15m ; retry
      8h  ; expire
      4m  ; neg cache TTL
    )
  '';

  zone-domain = ''
    $ORIGIN ${domain}.
    $TTL 5m
    ${soa}
    @ NS ns1
    @ A ${ip} 

    ns1 A ${ip}
    ${hostname} A ${ip}
    carbon A ${ip}
  '';

  zone-subnet = ''
    $ORIGIN ${subnetRevDomain}.
    $TTL 5m
    ${soa}
    @ NS ns1.${domain}.

    ${rev-ip4 ip}. PTR ${hostname}.${domain}.
  '';

in {
  networking = {
    inherit domain;
    hostId = "5a656e88";
    hostName = hostname;

    firewall = {
      interfaces.${lan} = {
        allowedUDPPorts = [ 53 ];
        allowedTCPPorts = [ 80 443 6566 ];
      };

      interfaces.${wan} = {
        allowedTCPPorts = [ 80 443 ];
      };
    };

    nameservers = lib.mkForce [ "127.0.0.1" ];

    interfaces = {
      ${wan}.useDHCP = true;

      ${lan} = {
        useDHCP = false;
        ipv4.addresses = [{
          address = ip;
          prefixLength = 24;
        }];
      };

      lo = {
        ipv4.addresses = [
          { address = "127.0.0.1"; prefixLength = 8; }
          { address = "127.0.0.2"; prefixLength = 8; }
        ];
      };
    };

    nat = {
      enable = true;
      internalIPs = [ "${subnet}.0/24" ];
      externalInterface = wan;
    };
  };

  services = {
    kea = {
      dhcp4 = {
        enable = true;
        settings = {
          rebind-timer = 2000;
          renew-timer = 1000;
          valid-lifetime = 4000;

          subnet4 = [{
            subnet = "${subnet}.0/24";
            pools = [{
              pool = "${subnet}.128 - ${subnet}.254";
            }];
          }];

          option-data = [
            {
              name = "routers";
              data = ip;
            }
            {
              name = "domain-name-servers";
              data = ip;
            }
          ];

          interfaces-config = {
            interfaces = [ lan ];
          };

          lease-database = {
            name = "/var/lib/kea/dhcp4.leases";
            persist = true;
            type = "memfile";
          };
        };
      };
    };

    nsd = {
      enable = true;
      port = 10053;
      zones = {
        "${domain}." = {
          data = zone-domain;
        };

        "${subnetRevDomain}." = {
          data = zone-subnet;
        };
      };
    };

    unbound = {
      enable = true;
      resolveLocalQueries = false;
      localControlSocketPath = "/run/unbound/unbound.ctl";

      settings = {
        server = {
          do-not-query-localhost = false;
          hide-identity = true;
          hide-version = true;
          so-reuseport = true;
          use-caps-for-id = false;
          verbosity = 2;

          interface = [ "${subnet}.1" "127.0.0.1" ];
          access-control = [ "${subnet}.0/24 allow" "127.0.0.0/8 allow" ];

          tls-cert-bundle = "/etc/ssl/certs/ca-certificates.crt";

          domain-insecure = [ domain subnetRevDomain ];
          local-zone = [
            "${domain}.          nodefault"
            "${subnetRevDomain}. nodefault"
          ];
        };

        stub-zone = [
          { name = domain; stub-addr = "::1@10053"; }
          { name = subnetRevDomain; stub-addr = "::1@10053"; }
        ];

        forward-zone = [{
          forward-addr = upstreamDns;
          forward-tls-upstream = true;
          name = ".";
        }];
      };
    };
  };
}
