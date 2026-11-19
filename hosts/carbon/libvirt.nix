{ pkgs, ... }:

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
  # <dhcp>/<dns>, so libvirt never starts dnsmasq. Guest addresses are static.
  # libvirtd-config only copies the stock XML when the file is absent, so this
  # also works on a fresh install.
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
    '';
  };
}
