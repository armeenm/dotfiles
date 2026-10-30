# tungsten

Headless Raspberry Pi 4, used only as a Tailscale exit node.

It runs `nixosMinimal` rather than the full home-manager profile, so the
system closure is roughly 1.2 GiB and the SD image is about 1.9 GiB before
first-boot expansion. There is no graphical stack, no emacs, and no
`nixos-rebuild` on the device.

## Build and flash

```sh
nix build .#nixosConfigurations.tungsten.config.system.build.sdImage
zstdcat result/sd-image/*.img.zst | sudo dd of=/dev/sdX bs=4M status=progress conv=fsync
```

The root partition expands to fill the card on first boot.

## First boot

Ethernet DHCP and SSH are enabled. Log in as `armeen` with the key from
`secrets/secrets.nix`, then join the tailnet:

```sh
sudo tailscale up --advertise-exit-node
```

Approve the exit node in the Tailscale admin console. The
`--advertise-exit-node` flag is also reapplied by `tailscaled-set.service`
on every boot.

## Update

The device has no `nixos-rebuild` on purpose. Update it from a host that can
build aarch64 (for example lithium):

```sh
nixos-rebuild switch --flake .#tungsten \
  --target-host armeen@tungsten --elevate=sudo
```
