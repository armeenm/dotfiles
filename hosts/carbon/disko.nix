# Install-time layout for carbon's system disk, mirroring today: a 487 MiB ESP
# at /boot and an ext4 root, with the filesystem and partition UUIDs pinned.
#
# The two NVMe drives holding the bcachefs tank are deliberately not part of
# this layout; a reinstall must not touch them.
#
# Not imported into the host yet. To reinstall from the hydrogen ISO:
#
#   sudo nix run github:nix-community/disko -- --flake ".#carbon" --mode destroy,format,mount
#   sudo nixos-install --flake ".#carbon"
{ device ? "/dev/disk/by-id/usb-Kingston_XS2000_50026B7282A379D1-0:0", ... }:
{
  disko.devices.disk.carbon = {
    type = "disk";
    inherit device;

    content = {
      type = "gpt";
      partitions = {
        ESP = {
          priority = 100;
          size = "487M";
          type = "EF00";
          uuid = "4aaf9ecb-3752-4cde-b85a-ac5f3be34c40";

          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            # mkfs.vfat volume ID for the current E777-4036.
            extraArgs = [ "-i" "E7774036" ];
          };
        };

        root = {
          size = "100%";
          uuid = "082c9afb-20fc-4b86-8350-45f7541da1a3";

          content = {
            type = "filesystem";
            format = "ext4";
            mountpoint = "/";
            mountOptions = [ "noatime" ];
            extraArgs = [ "-U" "580bb1f1-73ac-444a-b4ca-5e36b502cdb4" ];
          };
        };
      };
    };
  };
}
