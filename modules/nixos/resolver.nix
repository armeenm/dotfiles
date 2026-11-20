{ config, pkgs, ... }:

let
  warpLink = "CloudflareWARP";
  warpDevice = "sys-subsystem-net-devices-${warpLink}.device";
in

{
  # Tailscale owns /etc/resolv.conf while `accept-dns` is on, which leaves no
  # way to send a single domain somewhere else. With systemd-resolved enabled,
  # tailscaled configures DNS per link instead (MagicDNS and the tailnet's
  # split routes on tailscale0), and NetworkManager can attach a routing
  # domain such as `~armeen.xyz` to the LAN link. That is what makes the LAN
  # zone in Knot on carbon resolvable independently of which tailnet profile
  # is currently active.
  services.resolved.enable = true;
  networking.networkmanager.dns = "systemd-resolved";

  # The corporate Zero Trust client (Cloudflare WARP) publishes split DNS for
  # its internal zones from a local resolver on 127.0.2.2/3, and routes its
  # private subnets into the tunnel. It cannot register that resolver with
  # systemd-resolved: it fails to parse systemd 262's version string and falls
  # back to rewriting /etc/resolv.conf, which here is the resolved stub. The
  # internal zones are then never consulted, so those hosts stop resolving by
  # name while WARP is up.
  #
  # Worse, WARP's own connectivity check resolves `connectivity-check.warp-svc.`
  # through the system resolver. With that check failing, WARP loops forever
  # in "Connecting", tearing its tunnel interface down and recreating it every
  # few seconds, which also drops in-flight ssh sessions over the tunnel.
  #
  # Attach the zones WARP's policy actually resolves (its fallback domains that
  # carry a resolver), plus the synthetic check name, to the WARP link, so
  # resolution goes through WARP's resolver without giving it the default route
  # and without touching the LAN and tailnet routes above. resolved keeps
  # per-link DNS in memory only, so re-apply it whenever the link appears or
  # resolved restarts.
  systemd.services.warp-dns-split = {
    description = "Route the WARP split-DNS zones through its own resolver";

    wantedBy = [ warpDevice ];
    bindsTo = [ warpDevice ];
    after = [ "systemd-resolved.service" warpDevice ];
    requires = [ "systemd-resolved.service" ];
    partOf = [ "systemd-resolved.service" ];

    path = [
      config.systemd.package
      config.services.cloudflare-warp.package
      pkgs.coreutils
      pkgs.jq
    ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };

    script = ''
      ok=0
      for _ in {1..40}; do
        if [ -e /sys/class/net/${warpLink} ] &&
          resolvectl dns ${warpLink} 127.0.2.2 127.0.2.3; then
          ok=1
          break
        fi
        sleep 0.5
      done

      if [ "$ok" != 1 ]; then
        echo "warp-dns-split: ${warpLink} never appeared, nothing to do" >&2
        exit 0
      fi

      # Fallback domains that carry a resolver are the zones WARP answers for
      # itself; the ones without one tell WARP to defer to the system resolver
      # and must not be routed here.
      domains=""
      settings=$(warp-cli -j settings 2>/dev/null) || true
      if [ -n "$settings" ]; then
        domains=$(
          printf '%s' "$settings" |
            jq -r '.settings.fallback_domains[]? | select((.dns_servers | length) > 0) | "~" + .domain'
        ) || true
      fi

      resolvectl domain ${warpLink} '~warp-svc' ''${domains}
    '';
  };
}
