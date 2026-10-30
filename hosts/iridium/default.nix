{ ... }:

# Headless Raspberry Pi 3 Model B joined to the tailnet for remote access. It
# is not an exit node. The shared image setup lives in ../rpi.nix; see
# README.md for build and flash instructions.
{
  imports = [
    ../rpi.nix
  ];

  networking.hostName = "iridium";

  # A plain tailnet client: no forwarding and no exit-node advertisement.
  services.tailscale.useRoutingFeatures = "client";
}
