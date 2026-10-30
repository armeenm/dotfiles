{ config, lib, pkgs, modulesPath, ... }:

# Headless Raspberry Pi 4 used only as a Tailscale exit node. The image is
# built with:
#
#   nix build .#nixosConfigurations.tungsten.config.system.build.sdImage
#
# and written to the card with `zstdcat result/sd-image/*.img.zst | dd ...`.
{
  imports = [
    (modulesPath + "/installer/sd-card/sd-image.nix")
  ];

  nixpkgs.hostPlatform = "aarch64-linux";

  networking.hostName = "tungsten";

  # The card is the only writable disk; avoid unnecessary metadata writes.
  fileSystems."/".options = [ "noatime" ];

  system.stateVersion = lib.mkForce "26.05";

  boot = {
    # nixos-hardware defaults to the Raspberry Pi vendor kernel, which is not
    # in any binary cache and would be rebuilt on every flake check. Mainline
    # drives everything an exit node uses (SD, Ethernet, USB).
    kernelPackages = pkgs.linuxPackages_latest;

    # Mainline builds the Pi 4 SD host controller in, but keep the module
    # names in the initrd allowlist so a future kernel config that modularizes
    # them can still mount the root filesystem.
    initrd.availableKernelModules = [
      "sdhci"
      "sdhci_iproc"
    ];

    kernelParams = [ "quiet" ];

    loader = {
      grub.enable = false;

      generic-extlinux-compatible = {
        enable = true;
        # Keep only the current and previous generation in /boot.
        configurationLimit = 2;
      };
    };
  };

  # sd-image enables the installation-CD hardware profile, which pulls in the
  # generic linux-firmware package (over a gigabyte on its own). The Pi only
  # needs the board firmware staged by nixos-hardware.
  hardware = {
    enableAllHardware = lib.mkForce false;
    enableRedistributableFirmware = lib.mkForce false;

    raspberry-pi = {
      firmware.uboot.enable = true;

      # No display, camera or analog audio on an exit node.
      configtxt = {
        deviceTreeOverlays.all = lib.mkForce [ ];
        settings.all.dtparam = lib.mkForce [ ];
      };
    };
  };

  # Keep logs in RAM so journal writes do not wear the SD card. Logs are lost
  # on reboot, which is acceptable for a stateless exit node.
  services.journald.settings.Journal.Storage = "volatile";

  services.tailscale = {
    enable = true;
    openFirewall = true;

    # Enables IP forwarding for exit-node traffic.
    useRoutingFeatures = "server";

    # Persist the exit-node advertisement across restarts. Login itself is
    # still a one-time `tailscale up` after the first boot.
    extraSetFlags = [
      "--advertise-exit-node"
      "--accept-dns=false"
    ];
  };

  # tailscaled otherwise picks iptables-nft compatibility tables on Linux.
  # The host is nftables-only, so force its native nftables backend.
  systemd.services.tailscaled.environment.TS_DEBUG_FIREWALL_MODE = "nftables";

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

  sdImage = {
    compressImage = true;

    # U-Boot reads extlinux.conf and the kernel from /boot on the ext4 root
    # partition; the FAT partition only carries the Pi firmware and U-Boot.
    populateRootCommands = ''
      mkdir -p ./files/boot
      ${config.boot.loader.generic-extlinux-compatible.populateCmd} -c ${config.system.build.toplevel} -d ./files/boot
    '';
  };
}
