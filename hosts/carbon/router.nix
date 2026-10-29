{ config, lib, pkgs, ... }:

let
  rev-ip4 = with lib; ip4: "${concatStringsSep "." (reverseList (splitString "." ip4))}.in-addr.arpa";

  hostId = "196d325d";
  hostName = "carbon";
  domain = "armeen.xyz";

  wan = "enp90s0";
  lan = "enp87s0";
  aux = "enp2s0f0np0";

  prefix = "192.168.0";
  prefixRev = rev-ip4 prefix;

  subnet = "${prefix}.0/24";

  ip = "${prefix}.1";

  upstreamDns = [
    "2620:fe::11@853#dns.quad9.net"
    "2620:fe::fe:11@853#dns.quad9.net"
    "9.9.9.11@853#dns.quad9.net"
    "149.112.112.11@853#dns.quad9.net"
    "1.1.1.1@853#cloudflare-dns.com"
  ];

  # The zone contents live here and are generated into the store, so the
  # records are versioned with the rest of the configuration instead of
  # sitting in /var/lib/knot/zones where nothing tracked them.
  zoneRecords = ''
    @ NS ns1
    @ A ${ip}

    ns1 A ${ip}
    ${hostName} A ${ip}
    vault A ${ip}
    cobalt A ${ip}
    music A ${ip}
    media A ${ip}
  '';

  reverseZoneRecords = ''
    @ NS ns1.${domain}.

    ${rev-ip4 ip}. PTR ${hostName}.${domain}.
  '';

  # The SOA serial is a hash of the records, so changing the zone changes the
  # serial without anyone having to remember to bump it. knot's default
  # serial-policy ("increment") keeps it monotonic in the journal if the hash
  # happens to go down.
  zoneSerial = records: lib.fromHexString (builtins.substring 0 8 (builtins.hashString "sha256" records));

  mkZone = origin: records: ''
    $ORIGIN ${origin}.
    $TTL 5m
    @ SOA ns1 hostmaster (
      ${toString (zoneSerial records)} ; serial
      4h  ; refresh
      15m ; retry
      8h  ; expire
      4m  ; neg cache TTL
    )
    ${records}
  '';

  zoneFile = pkgs.writeText "${domain}.zone" (mkZone domain zoneRecords);
  reverseZoneFile = pkgs.writeText "${prefixRev}.zone" (mkZone prefixRev reverseZoneRecords);

  # DHCP registrations are signed with a TSIG key that is generated on first
  # boot and kept out of the store (both knot and kea can read it by group).
  tsigKeyId = "ddns-key";
  tsigKeyDir = "/run/knot-ddns";

in
{
  networking = {
    inherit hostName hostId domain;

    firewall.enable = false;
    nat.enable = false;

    nftables = {
      enable = true;
      ruleset = ''
        table inet filter {
          chain output {
            type filter hook output priority 100; policy accept;
          }

          chain input {
            type filter hook input priority filter; policy drop;

            iifname "lo" accept

            # Conntrack first: replies to our own connections are always fine,
            # and invalid packets (bad state, out-of-window) are dropped early
            # instead of being matched by the interface rules below.
            ct state established,related counter accept
            ct state invalid counter drop

            # libvirt guests on the default NAT bridge
            iifname "virbr0" accept

            # Jellyfin binds 0.0.0.0:8096; only nginx on loopback should reach
            # it, so the LAN cannot skip TLS and the vhost headers.
            iifname { "${lan}", "${aux}" } tcp dport 8096 counter drop

            iifname "${aux}" counter accept
            iifname "${lan}" counter accept

            # Tailscale peers are authenticated by WireGuard.
            iifname "tailscale0" counter accept

            # Rate-limit new SSH connections before accepting them. sshd's
            # PerSourcePenalties handles failed-auth clients; this keeps the
            # pre-auth work (key exchange) bounded as well.
            iifname "${wan}" tcp dport 22 ct state new \
              limit rate over 6/minute burst 5 packets \
              counter drop comment "ssh connection flood"
            iifname "${wan}" tcp dport 22 counter accept

            iifname "${wan}" tcp dport 443 counter accept
            iifname "${wan}" tcp dport 80 counter accept
            iifname "${wan}" drop
          }

          chain forward {
            type filter hook forward priority filter; policy drop;

            ct state invalid counter drop

            # libvirt guests on the default NAT bridge
            iifname "virbr0" accept
            oifname "virbr0" accept

            # aux is only a backup path to reach the host itself, so it has no
            # forward rules.
            iifname {
              "${lan}",
            } oifname {
              "${wan}",
            } counter accept comment "Allow trusted LAN to WAN"

            iifname {
              "${wan}",
            } oifname {
              "${lan}",
            } ct state established,related counter accept comment "Allow established back to LANs"
          }
        }

        table ip nat {
          chain output {
            type nat hook output priority filter; policy accept;
          }

          chain prerouting {
            type nat hook prerouting priority -100; policy accept;
          }

          chain postrouting {
            type nat hook postrouting priority filter; policy accept;
            oifname "${wan}" masquerade
          }
        }
      '';
    };

    nameservers = lib.mkForce [ "::1" ];

    interfaces = {
      ${aux}.useDHCP = true;
      ${wan}.useDHCP = true;

      ${lan} = {
        useDHCP = false;
        ipv4.addresses = [{
          address = ip;
          prefixLength = 24;
        }];
      };
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
          ddns-qualifying-suffix = domain;

          subnet4 = [
            {
              id = 1;
              inherit subnet;
              pools = [ { pool = "${prefix}.128 - ${prefix}.254"; } ];
            }
          ];

          option-data = [
            {
              name = "routers";
              data = ip;
            }
            {
              name = "domain-name-servers";
              data = ip;
            }
            {
              name = "domain-name";
              data = domain;
            }
            {
              name = "domain-search";
              data = domain;
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

          dhcp-ddns = {
            enable-updates = true;
            server-ip = "::1";
            server-port = 53001;
            ncr-format = "JSON";
            ncr-protocol = "UDP";
          };
        };
      };

      dhcp-ddns = {
        enable = true;
        settings = {
          dns-server-timeout = 100;
          # Must match services.kea.dhcp4's dhcp-ddns.server-ip.
          ip-address = "::1";
          ncr-format = "JSON";
          ncr-protocol = "UDP";
          port = 53001;

          "tsig-keys" = [
            {
              name = tsigKeyId;
              algorithm = "hmac-sha256";
              "secret-file" = "${tsigKeyDir}/secret";
            }
          ];

          forward-ddns = {
            ddns-domains = [
              {
                name = "${domain}.";
                key-name = tsigKeyId;
                dns-servers = [
                  {
                    ip-address = "::1";
                    port = 10053;
                  }
                ];
              }
            ];
          };
        };
      };
    };

    knot = {
      enable = true;

      # Included before the rest of the config, so the key exists by the time
      # the ACL that references it is parsed.
      keyFiles = [ "${tsigKeyDir}/tsig.conf" ];

      settings = {
        server.listen = "::@10053";

        log = [{
            target = "syslog";
            any = "info";
        }];

        acl = [
          {
            id = "update_acl";
            address = "::1";
            key = tsigKeyId;
            action = "update";
          }
        ];

        zone = [
          {
            inherit domain;
            storage = "/var/lib/knot/zones";
            file = "${domain}.zone";
            acl = "update_acl";
            # The zone file is a store symlink; knot must only write the
            # journal, never the file.
            zonefile-sync = "-1";
          }
          {
            domain = prefixRev;
            storage = "/var/lib/knot/zones";
            file = "${prefixRev}.zone";
            zonefile-sync = "-1";
          }
        ];
      };
    };

    unbound = {
      enable = true;
      resolveLocalQueries = false;
      localControlSocketPath = "/run/unbound/unbound.ctl";

      settings = {
        server = {
          # Needed for the local knot stub below; unbound otherwise refuses to
          # send queries to loopback addresses.
          do-not-query-localhost = false;
          hide-identity = true;
          hide-version = true;
          so-reuseport = true;
          verbosity = 1;

          # Query statistics are available on demand via
          # `unbound-control stats_noreset`, so there is no need to log every
          # name that the LAN resolves.
          extended-statistics = true;

          interface = [ ip "::1" ];
          access-control = [ "${subnet} allow" "::1 allow" ];

          tls-cert-bundle = "/etc/ssl/certs/ca-certificates.crt";

          domain-insecure = [ domain prefixRev ];
          # TODO: Use the revPrefix stuff.
          local-zone = [
            "${domain}.    nodefault"
            "168.192.in-addr.arpa. nodefault"
          ];
        };

        stub-zone = [
          { name = domain; stub-addr = "::1@10053"; }
          { name = "168.192.in-addr.arpa"; stub-addr = "::1@10053"; }
        ];

        forward-zone = [
          {
            forward-addr = upstreamDns;
            forward-tls-upstream = true;
            name = ".";
          }
          # These live on Cloudflare, not in the local knot zone. Without an
          # explicit forward-zone the armeen.xyz stub below turns them into
          # NXDOMAIN, so carbon and LAN clients could not resolve the cache.
          {
            forward-addr = upstreamDns;
            forward-tls-upstream = true;
            name = "cache.armeen.xyz";
          }
          {
            forward-addr = upstreamDns;
            forward-tls-upstream = true;
            name = "niks3.armeen.xyz";
          }
        ];
      };
    };
  };

  systemd = {
    # The zone files are generated from the records above and symlinked into
    # knot's storage directory, so they are versioned with the configuration.
    # knot only writes its journal (zonefile-sync = -1).
    tmpfiles.rules = [
      "d /var/lib/knot/zones 0750 knot knot -"
      "L+ /var/lib/knot/zones/${domain}.zone - - - - ${zoneFile}"
      "L+ /var/lib/knot/zones/${prefixRev}.zone - - - - ${reverseZoneFile}"
    ];

    services.knot = {
      # Before= on the generator only orders units already in the transaction;
      # a switch that restarts knot.service on its own has to pull it in, or
      # the include file is missing when knot parses its config.
      wants = [ "knot-ddns-key.service" ];
      after = [ "knot-ddns-key.service" ];

      # Reload the zones whenever their generated contents change.
      restartTriggers = [ zoneFile reverseZoneFile ];
    };

    # One shared TSIG key for DHCP registrations. It is generated once, kept in
    # /var/lib/knot-ddns, and rendered into both knot's include file and Kea's
    # DDNS config so the secret never ends up in the world-readable store.
    services.knot-ddns-key = {
      description = "Generate the TSIG key for DHCP dynamic DNS updates";
      wantedBy = [ "multi-user.target" ];
      before = [ "knot.service" "kea-dhcp-ddns-server.service" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        StateDirectory = "knot-ddns";
        StateDirectoryMode = "0750";
        RuntimeDirectory = "knot-ddns";
        RuntimeDirectoryMode = "0755";
      };

      script = ''
        set -eu
        umask 0027

        secret=/var/lib/knot-ddns/secret
        if [ ! -s "$secret" ]; then
          ${pkgs.openssl}/bin/openssl rand -base64 32 > "$secret"
        fi
        chown root:knot "$secret"
        chmod 0640 "$secret"

        {
          printf 'key:\n'
          printf '  - id: ${tsigKeyId}\n'
          printf '    algorithm: hmac-sha256\n'
          printf '    secret: "%s"\n' "$(cat "$secret")"
        } > ${tsigKeyDir}/tsig.conf
        chown root:knot ${tsigKeyDir}/tsig.conf
        chmod 0640 ${tsigKeyDir}/tsig.conf

        # Kea reads this one via tsig-keys.secret-file.
        install -m 0640 -o root -g kea "$secret" ${tsigKeyDir}/secret
      '';
    };

    # The DDNS config points at the secret file, so the service only needs to
    # start after it exists.
    services.kea-dhcp-ddns-server = {
      wants = [ "knot-ddns-key.service" ];
      after = [ "knot-ddns-key.service" ];
    };
  };
}
