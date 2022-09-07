{ config, lib, domain, ... }:

let
  esc-nl = str: builtins.replaceStrings [ "\n" ] [ "\\n" ] str;
  rev-ip4 = with lib; ip4: "${concatStringsSep "." (reverseList (splitString "." ip4))}.in-addr.arpa";

  hostId = "5a656e88";

  wan = "enp4s0f1";
  lan = "enp4s0f0";

  prefix = "192.168.0";
  prefixRev = rev-ip4 prefix;

  subnet = "${prefix}.0/24";

  ip = "${prefix}.1";

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
    ${hostName} A ${ip}
    carbon A ${ip}
  '';

  zone-subnet = ''
    $ORIGIN ${prefixRev}.
    $TTL 5m
    ${soa}
    @ NS ns1.${domain}.

    ${rev-ip4 ip}. PTR ${hostName}.${domain}.
  '';

in
{
  networking = {
    firewall = {
      allowPing = false;

      interfaces.${lan} = {
        allowedUDPPorts = [ 53 ];
        allowedTCPPorts = [ 80 443 6566 ];
      };

      interfaces.${wan} = {
        allowedTCPPorts = [ 80 443 ];
      };

      extraCommands = ''
        iptables -A nixos-fw -i ${lan} -p icmp -j nixos-fw-accept
      '';
    };

    nameservers = lib.mkForce [ "::1" ];

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
        ];
      };
    };

    nat = {
      enable = true;
      internalIPs = [ subnet ];
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
            inherit subnet;
            pools = [{
              pool = "${prefix}.128 - ${prefix}.254";
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
      interfaces = [ "::1" ];
      port = 10053;

      zones = {
        "${domain}." = {
          data = zone-domain;
        };

        "${prefixRev}." = {
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

          interface = [ ip "::1" ];
          access-control = [ "${subnet} allow" "::1 allow" ];

          tls-cert-bundle = "/etc/ssl/certs/ca-certificates.crt";

          domain-insecure = [ domain prefixRev ];
          local-zone = [
            "${domain}.    nodefault"
            "${prefixRev}. nodefault"
          ];
        };

        stub-zone = [
          { name = domain; stub-addr = "::1@10053"; }
          { name = prefixRev; stub-addr = "::1@10053"; }
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
