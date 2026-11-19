# Install-time layout for argentum, mirroring the disk as it exists today:
# an ESP at /boot and a natively-encrypted bcachefs root, with the filesystem
# and partition UUIDs pinned so the host's fileSystems keep working after a
# reinstall.
#
# Not imported into the host yet. To reinstall from the hydrogen ISO:
#
#   printf '%s' "$BCACHEFS_PASSPHRASE" > /tmp/bcachefs.key
#   sudo nix run github:nix-community/disko -- --flake ".#argentum" --mode destroy,format,mount
#   sudo nixos-install --flake ".#argentum"
#
# The passphrase file is only read while formatting/mounting; it is not part of
# the Nix store or the installed system.
{ device ? "/dev/disk/by-id/nvme-WD_BLACK_SN7100_2TB_254728806072", ... }:
{
  disko.devices = {
    disk.argentum = {
      type = "disk";
      inherit device;

      content = {
        type = "gpt";
        partitions = {
          # The running disk has the root first and the ESP second. disko puts
          # the 100% partition last, so the ESP becomes partition 1 here; the
          # pinned PARTUUIDs keep both filesystems independent of the order.
          ESP = {
            priority = 100;
            size = "3813M"; # 3,999,264,768 bytes; today's partition is 3999268864
            type = "EF00";
            uuid = "a6d66c02-904d-46b8-8aac-f46ee784b638";

            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [ "fmask=0077" "dmask=0077" ];
              # mkfs.vfat volume ID for the current A475-3082.
              extraArgs = [ "-i" "A4753082" ];
            };
          };

          root = {
            size = "100%";
            uuid = "26016d5b-ed58-4c58-8a3a-ac59237c899e";

            content = {
              type = "bcachefs";
              filesystem = "root";
              # bcachefs refuses an empty --label; the running filesystem has
              # no device groups, so any single-device label is equivalent.
              label = "root";
            };
          };
        };
      };
    };

    bcachefs_filesystems.root = {
      type = "bcachefs_filesystem";
      uuid = "fe2129dc-34fe-4386-bc5a-e6b595b4b59e";
      mountpoint = "/";
      passwordFile = "/tmp/bcachefs.key";
    };
  };
}
