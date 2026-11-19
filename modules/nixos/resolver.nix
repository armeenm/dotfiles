{ ... }:

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
}
