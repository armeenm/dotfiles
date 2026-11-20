{ config, pkgs, ... }:

let
  # Kea serves DHCP on virbr0 as well. A bridge only counts as "running" when
  # a port gives it a carrier, and Kea skips interfaces that are not running.
  # Keep a dummy port attached so the bridge stays running with no VM up, then
  # reload Kea so it opens the socket on the new interface.
  networkHook = pkgs.writeShellScript "libvirt-network-virbr0-dummy" ''
    # $1 is the network name, $2 the action (start, started, stopped, ...).
    [ "$1" = "default" ] || exit 0
    case "$2" in
      started)
        ${pkgs.iproute2}/bin/ip link add virbr0-dummy type dummy 2>/dev/null || true
        ${pkgs.iproute2}/bin/ip link set virbr0-dummy master virbr0 2>/dev/null || true
        ${pkgs.iproute2}/bin/ip link set virbr0-dummy up
        ${config.systemd.package}/bin/systemctl reload kea-dhcp4-server.service 2>/dev/null || true
        ;;
      stopped)
        ${pkgs.iproute2}/bin/ip link del virbr0-dummy 2>/dev/null || true
        ;;
    esac
  '';
in
{
  virtualisation.libvirtd.enable = true;

  # libvirt's stock "default" network includes a DHCP range, so libvirt starts
  # dnsmasq for it. dnsmasq's DHCP socket is a wildcard socket scoped to
  # virbr0 (0.0.0.0%virbr0:67), which holds UDP/67 for the entire host; Kea's
  # fallback socket on 192.168.0.1:67 then fails with EADDRINUSE and the LAN
  # gets no DHCP. Whichever daemon binds first at boot wins, and the other
  # fails: the LAN or the VM network.
  #
  # The repo owns the network definition instead: a NAT network with no
  # <dhcp> and <dns enable='no'>, so libvirt never starts dnsmasq. Kea serves
  # DHCP on virbr0 itself. libvirtd-config only copies the stock XML when the
  # file is absent, so this also works on a fresh install.
  systemd.services.libvirt-network-default = {
    description = "Install the libvirt default network definition (NAT, no DHCP)";
    wantedBy = [ "multi-user.target" ];
    # libvirtd.socket is enabled, so libvirtd can be socket-activated before
    # multi-user.target; install the definition before the socket exists.
    before = [ "libvirtd.socket" "libvirtd-config.service" "libvirtd.service" ];
    path = [ pkgs.coreutils ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };

    script = ''
      install -d -m 0755 /var/lib/libvirt/qemu/networks/autostart
      install -m 0644 ${./libvirt-net.xml} /var/lib/libvirt/qemu/networks/default.xml
      ln -sfn ../default.xml /var/lib/libvirt/qemu/networks/autostart/default.xml
      # Install the network hook here as well. libvirtd-config is a oneshot that
      # activation does not re-run when only the hook changes, so a deploy would
      # otherwise leave the hook uninstalled until the next boot. This script
      # embeds the hook, so its unit changes whenever the hook does and runs.
      install -d -m 0755 /var/lib/libvirt/hooks/network.d
      ln -sfn ${networkHook} /var/lib/libvirt/hooks/network.d/virbr0-dummy
    '';
  };

  # The declarative hook keeps the stock install path working on a fresh boot.
  virtualisation.libvirtd.hooks.network.virbr0-dummy = networkHook;
}
