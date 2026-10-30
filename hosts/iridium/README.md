# iridium

Headless Raspberry Pi 3 Model B, joined to the tailnet for remote access. It
is not an exit node.

It runs `nixosMinimal` rather than the full home-manager profile, so the
system closure is small and there is no graphical stack, no emacs, and no
`nixos-rebuild` on the device.

## Build and flash

```sh
nix build .#nixosConfigurations.iridium.config.system.build.sdImage
zstdcat result/sd-image/*.img.zst | sudo dd of=/dev/sdX bs=4M status=progress conv=fsync
```

The root partition expands to fill the card on first boot.

## First boot

Ethernet DHCP and SSH are enabled. Log in as `armeen` with the key from
`secrets/secrets.nix`, then join the tailnet:

```sh
sudo tailscale up --accept-dns=false
```

DNS stays local on the node; other tailnet machines can still reach it by
name through MagicDNS.

## Update

The device has no `nixos-rebuild` on purpose. Deploy from a host that can
build aarch64 (for example lithium):

```sh
deploy .#iridium
```

The node sets `remoteBuild = false`, so the build happens on the deploying
machine rather than on the Pi.
