{ config, lib, pkgs, modulesPath, ... }:

# Headless Raspberry Pi 3 Model B joined to the tailnet for remote access. It
# is not an exit node. The image is built with:
#
#   nix build .#nixosConfigurations.iridium.config.system.build.sdImage
#
# and written to the card with `zstdcat result/sd-image/*.img.zst | dd ...`.
{
  imports = [
    (modulesPath + "/installer/sd-card/sd-image.nix")
  ];

  nixpkgs.hostPlatform = "aarch64-linux";

  networking.hostName = "iridium";

  # The card is the only writable disk; avoid unnecessary metadata writes.
  fileSystems."/".options = [ "noatime" ];

  system.stateVersion = lib.mkForce "26.05";

  boot = {
    # nixos-hardware defaults to the Raspberry Pi vendor kernel, which is not
    # in any binary cache and would be rebuilt on every flake check. Mainline
    # supports the Pi 3's SD card and USB Ethernet.
    kernelPackages = pkgs.linuxPackages_latest;

    # Keep the SD host controller modules available even if a future kernel
    # config modularizes them.
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

      # No display, camera or analog audio on a headless node.
      configtxt = {
        deviceTreeOverlays.all = lib.mkForce [ ];
        settings.all.dtparam = lib.mkForce [ ];
      };
    };
  };

  # Keep logs in RAM so journal writes do not wear the SD card. Logs are lost
  # on reboot, which is acceptable for a stateless node.
  services.journald.settings.Journal.Storage = "volatile";

  services.tailscale = {
    enable = true;
    openFirewall = true;

    # A plain tailnet client: no forwarding and no exit-node advertisement.
    useRoutingFeatures = "client";
  };

  # tailscaled otherwise picks iptables-nft compatibility tables on Linux.
  # The host is nftables-only, so force its native nftables backend.
  systemd.services.tailscaled.environment.TS_DEBUG_FIREWALL_MODE = "nftables";

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
