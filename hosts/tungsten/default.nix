{ pkgs, ... }:

# Headless Raspberry Pi 4 used only as a Tailscale exit node. The shared image
# setup lives in ../rpi.nix; see README.md for build and flash instructions.
{
  imports = [
    ../rpi.nix
  ];

  networking.hostName = "tungsten";

  services.tailscale = {
    # Enables IP forwarding for exit-node traffic.
    useRoutingFeatures = "server";

    # Persist the exit-node advertisement across restarts. Login itself is
    # still a one-time `tailscale up` after the first boot.
    extraSetFlags = [
      "--advertise-exit-node"
      "--accept-dns=false"
    ];
  };

  # Tailscale asks for this on exit nodes; it improves forwarded UDP
  # throughput. Wait for the interface device so ethtool has a link to act on.
  systemd.services.tailscale-gro = {
    description = "Enable UDP GRO forwarding for Tailscale exit-node traffic";
    wantedBy = [ "multi-user.target" ];
    wants = [ "sys-subsystem-net-devices-end0.device" ];
    after = [ "sys-subsystem-net-devices-end0.device" ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };

    script = ''
      ${pkgs.ethtool}/bin/ethtool -K end0 rx-udp-gro-forwarding on rx-gro-list off
    '';
  };
}
