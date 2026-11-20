# The encrypted bcachefs tank and everything that lives on it.
#
# The tank is one bcachefs filesystem with native encryption on two NVMe
# members. It must not unlock itself: there is no TPM, no keyfile and no
# initrd networking. A human SSHes in and runs `sudo tank-unlock`, which
# unlocks the filesystem and starts tank.target.
#
# Before this file existed /srv/tank was an undeclared directory on the
# unencrypted root disk. Services started at boot and wrote there (Vaultwarden
# backups, postgres dumps, the restic snapshot of "the tank"), so while the
# tank was locked they produced shadow copies on / and backups of the wrong
# data. The mount is now declared noauto, every consumer is gated on it and
# pulled in by tank.target, and the unmounted directory is a root-owned
# mode-0555 placeholder that no service user can write into.
{ config, lib, pkgs, ... }:

let
  tankUuid = "63afad52-6581-4e67-a769-035d16c4c080";
  tankDevice = "/dev/disk/by-uuid/${tankUuid}";
  tankMount = "/srv/tank";
  exportMount = "/export/tank";

  # Unit options shared by every consumer of the tank: pull in the mount,
  # order after it, and skip cleanly (instead of touching the root-disk
  # placeholder) when the tank is locked.
  tankUnitConfig = {
    RequiresMountsFor = [ tankMount ];
    ConditionPathIsMountPoint = tankMount;
  };

  # Consumers are started by tank.target after the unlock, never at boot, and
  # stopping the target stops them so nothing keeps running on a vanished
  # mount.
  tankWantedBy = lib.mkForce [ "tank.target" ];
  tankPartOf = [ "tank.target" ];

  systemctl = "${config.systemd.package}/bin/systemctl";
in
{
  fileSystems = {
    # noauto keeps srv-tank.mount out of local-fs.target, so nothing tries to
    # mount the tank before the passphrase has been supplied. nofail stops a
    # failed mount attempt from blocking the boot.
    "/srv/tank" = {
      device = tankDevice;
      fsType = "bcachefs";
      options = [ "noauto" "nofail" ];
    };

    # A local bind mount, not an NFS filesystem mounted from itself. The
    # requires-mounts-for option makes export-tank.mount depend on the real
    # tank instead of binding the root-disk placeholder.
    "/export/tank" = {
      device = tankMount;
      fsType = "none";
      options = [
        "bind"
        "noauto"
        "x-systemd.requires-mounts-for=${tankMount}"
      ];
    };
  };

  # While the tank is locked this directory is on the root disk. 0555 root
  # root means no service user can write into it, so a unit that ignores the
  # mount can no longer recreate the shadow tree. When the tank is mounted,
  # --create during a rebuild also applies this mode to the tank root; root
  # keeps CAP_DAC_OVERRIDE and subdirectories are untouched.
  systemd.tmpfiles.rules = [
    "d ${tankMount} 0555 root root -"
    "d ${exportMount} 0555 root root -"
  ];

  # The vaultwarden module asks tmpfiles for its backup directory at every
  # boot, which is how a writable empty tree ended up on the root disk.
  # tank-init creates it on the real filesystem after the mount instead.
  systemd.tmpfiles.settings."10-vaultwarden" = lib.mkForce { };

  # navidrome's module does the same for its music folder. The library is the
  # human's data on the tank, so nothing here creates or chowns it; dropping
  # the rule is enough to keep the path off the root disk.
  systemd.tmpfiles.settings.navidromeDirs."/srv/tank/armeen/music" = lib.mkForce { };

  # Everything the tank carries comes up as one target. It is deliberately
  # wanted by nothing: `systemctl start tank.target` (normally through
  # tank-unlock) is the only way in, and AllowIsolate keeps it usable as a
  # maintenance target.
  systemd.targets.tank = {
    description = "Unlocked tank filesystem and its services";
    requires = [ "srv-tank.mount" "export-tank.mount" ];
    after = [ "srv-tank.mount" "export-tank.mount" ];
    unitConfig.AllowIsolate = true;
    wantedBy = [ ];
  };

  # Directories the consumers expect, created only on the mounted filesystem.
  systemd.services.tank-init = {
    description = "Create tank service directories";
    wantedBy = [ "tank.target" ];
    unitConfig = tankUnitConfig;
    serviceConfig.Type = "oneshot";
    script = ''
      ${pkgs.coreutils}/bin/install -d -m 0770 -o vaultwarden -g vaultwarden ${tankMount}/vaultwarden
      ${pkgs.coreutils}/bin/install -d -m 0700 -o postgres -g postgres ${tankMount}/postgresql
    '';
  };

  systemd.services = {
    # `bcachefs unlock -c` exits 0 for both the locked and the unlocked
    # state, so the module's ExecCondition always passes and the unit prompts
    # even after tank-unlock has already unlocked the device. Match the
    # message instead: the unit only runs while the filesystem is locked.
    "unlock-bcachefs-srv-tank" = {
      serviceConfig.ExecCondition = lib.mkForce (
        "${pkgs.bash}/bin/bash -c \"! ${pkgs.bcachefs-tools}/bin/bcachefs unlock -c ${tankDevice} | ${pkgs.gnugrep}/bin/grep -q unlocked\""
      );
    };

    # Vaultwarden's own data stays in /var/lib/vaultwarden; only the oneshot
    # backup writes to the tank. It is triggered by its timer (which is
    # wanted by tank.target), not started directly, so unlocking does not
    # block on a long backup.
    backup-vaultwarden = {
      partOf = tankPartOf;
      unitConfig = tankUnitConfig;
      requires = lib.mkAfter [ "tank-init.service" ];
      after = lib.mkAfter [ "tank-init.service" ];
    };

    restic-backups-b2 = {
      partOf = tankPartOf;
      unitConfig = tankUnitConfig;
      after = lib.mkAfter [ "tank-init.service" ];
    };

    postgresqlBackup = {
      partOf = tankPartOf;
      unitConfig = tankUnitConfig;
      requires = lib.mkAfter [ "tank-init.service" ];
      after = lib.mkAfter [ "tank-init.service" ];
    };

    jellyfin = {
      wantedBy = tankWantedBy;
      partOf = tankPartOf;
      unitConfig = tankUnitConfig;
    };

    navidrome = {
      wantedBy = tankWantedBy;
      partOf = tankPartOf;
      unitConfig = tankUnitConfig;
    };

    cobalt-dufs = {
      wantedBy = tankWantedBy;
      partOf = tankPartOf;
      unitConfig = tankUnitConfig;
    };

    # nfs-server never starts on the root-disk placeholder: the only thing it
    # exports is the bind mount of the tank. Starting it after the mount lets
    # its own exportfs run pick up the real filesystem.
    nfs-server = {
      wantedBy = tankWantedBy;
      partOf = tankPartOf;
      unitConfig = {
        RequiresMountsFor = [ tankMount exportMount ];
        ConditionPathIsMountPoint = [ tankMount exportMount ];
      };
    };
  };

  # The timers start with the target. Persistent restic/postgres timers catch
  # up immediately on start, so the first unlock after a reboot takes a
  # backup, and while the tank is locked the timers do not exist at all.
  systemd.timers = {
    backup-vaultwarden = {
      wantedBy = tankWantedBy;
      partOf = tankPartOf;
    };
    postgresqlBackup = {
      wantedBy = tankWantedBy;
      partOf = tankPartOf;
      # Unlike the restic and vaultwarden timers, the postgres module's timer
      # is not persistent. Make it catch up when tank.target starts so the
      # first unlock still takes a dump without blocking the target.
      timerConfig.Persistent = true;
    };
    restic-backups-b2 = {
      wantedBy = tankWantedBy;
      partOf = tankPartOf;
    };
  };

  environment.systemPackages = [
    (pkgs.writeShellScriptBin "tank-unlock" ''
      set -euo pipefail

      if [ "$(id -u)" -ne 0 ]; then
        echo "tank-unlock: must run as root (try: sudo tank-unlock)" >&2
        exit 1
      fi

      device="${tankDevice}"

      if ${pkgs.util-linux}/bin/findmnt --mountpoint ${tankMount} >/dev/null 2>&1; then
        echo "tank-unlock: ${tankMount} is already mounted; skipping the unlock"
      else
        # `bcachefs unlock -c` exits 0 for both the locked and the unlocked
        # state, so check the message. The mount unit's unlock helper uses the
        # same check and skips once the key is in the keyring, so this prompt
        # is the only one.
        if ${pkgs.bcachefs-tools}/bin/bcachefs unlock -c "$device" | ${pkgs.gnugrep}/bin/grep -q "and locked"; then
          # Prompts on the controlling terminal and keeps the key in root's
          # keyring for this boot only.
          ${pkgs.bcachefs-tools}/bin/bcachefs unlock "$device"
        else
          echo "tank-unlock: $device is already unlocked; skipping the unlock"
        fi
        ${systemctl} start srv-tank.mount
      fi

      ${systemctl} start tank.target

      # A remount hands the kernel a new superblock while the export table
      # still refers to the old one; re-read the exports if NFS is already up.
      if ${systemctl} --quiet is-active nfs-server.service; then
        ${pkgs.nfs-utils}/bin/exportfs -ra || true
      fi

      ${pkgs.util-linux}/bin/findmnt --mountpoint ${tankMount}
      ${pkgs.util-linux}/bin/findmnt --mountpoint ${exportMount}
    '')
  ];
}
