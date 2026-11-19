{ lib, pkgs, diskoModule, layouts }:
let
  mkLayout = layout: device:
    lib.nixosSystem {
      system = "x86_64-linux";

      modules = [
        diskoModule
        (layout { inherit device; })
        {
          disko.enableConfig = false;
        }
      ];
    };

  argentum = mkLayout layouts.argentum
    "/dev/disk/by-id/virtio-disko-argentum";
  carbon = mkLayout layouts.carbon
    "/dev/disk/by-id/virtio-disko-carbon";
in
pkgs.testers.runNixOSTest {
  name = "disko-layouts";

  nodes.machine = {
    boot = {
      kernelModules = [ "bcachefs" ];
      supportedFilesystems = [ "bcachefs" ];
    };

    virtualisation = {
      memorySize = 1536;

      emptyDiskImages = [
        {
          size = 8192;
          driveConfig.deviceExtraOpts.serial = "disko-argentum";
        }
        {
          size = 2048;
          driveConfig.deviceExtraOpts.serial = "disko-carbon";
        }
      ];
    };
  };

  testScript = ''
    machine.succeed("echo -n 'test passphrase' > /tmp/bcachefs.key")

    # argentum: encrypted bcachefs root + ESP, with the installed UUIDs.
    machine.succeed("${argentum.config.system.build.format}/bin/disko-format")

    assert machine.succeed("blkid -s TYPE -o value /dev/disk/by-partuuid/26016d5b-ed58-4c58-8a3a-ac59237c899e").strip() == "bcachefs"
    assert machine.succeed("blkid -s UUID -o value /dev/disk/by-partuuid/26016d5b-ed58-4c58-8a3a-ac59237c899e").strip() == "fe2129dc-34fe-4386-bc5a-e6b595b4b59e"
    assert machine.succeed("blkid -s TYPE -o value /dev/disk/by-partuuid/a6d66c02-904d-46b8-8aac-f46ee784b638").strip() == "vfat"
    assert machine.succeed("blkid -s UUID -o value /dev/disk/by-partuuid/a6d66c02-904d-46b8-8aac-f46ee784b638").strip() == "A475-3082"
    assert "encrypted and locked" in machine.succeed("${pkgs.bcachefs-tools}/bin/bcachefs unlock -c /dev/disk/by-uuid/fe2129dc-34fe-4386-bc5a-e6b595b4b59e")

    machine.succeed("${argentum.config.system.build.mount}/bin/disko-mount")
    assert machine.succeed("findmnt -no FSTYPE /mnt").strip() == "bcachefs"
    assert machine.succeed("findmnt -no FSTYPE /mnt/boot").strip() == "vfat"
    machine.succeed("${argentum.config.system.build.unmount}/bin/disko-unmount")

    # carbon: ext4 root + ESP; the tank disks are not in this layout.
    machine.succeed("${carbon.config.system.build.format}/bin/disko-format")

    assert machine.succeed("blkid -s TYPE -o value /dev/disk/by-partuuid/082c9afb-20fc-4b86-8350-45f7541da1a3").strip() == "ext4"
    assert machine.succeed("blkid -s UUID -o value /dev/disk/by-partuuid/082c9afb-20fc-4b86-8350-45f7541da1a3").strip() == "580bb1f1-73ac-444a-b4ca-5e36b502cdb4"
    assert machine.succeed("blkid -s TYPE -o value /dev/disk/by-partuuid/4aaf9ecb-3752-4cde-b85a-ac5f3be34c40").strip() == "vfat"
    assert machine.succeed("blkid -s UUID -o value /dev/disk/by-partuuid/4aaf9ecb-3752-4cde-b85a-ac5f3be34c40").strip() == "E777-4036"

    machine.succeed("${carbon.config.system.build.mount}/bin/disko-mount")
    assert machine.succeed("findmnt -no FSTYPE /mnt").strip() == "ext4"
    assert machine.succeed("findmnt -no FSTYPE /mnt/boot").strip() == "vfat"
    machine.succeed("${carbon.config.system.build.unmount}/bin/disko-unmount")
  '';
}
