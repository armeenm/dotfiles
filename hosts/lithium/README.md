# Drive Setup

```
DISK=/dev/disk/by-id/...

sgdisk -o $DISK
sgdisk -n2:1M:+4096M -t2:EF00 $DISK
sgdisk -n1:0:0 -t1:BF01 $DISK

zpool create          \
-O mountpoint=none  \
-O atime=off        \
-O compression=lz4  \
-O xattr=sa         \
-O acltype=posixacl \
-o ashift=12        \
rpool $DISK-part1

zfs create                  \
-o mountpoint=legacy      \
-o encryption=aes-256-gcm \
-o keyformat=passphrase   \
rpool/root
zfs create -o mountpoint=legacy rpool/root/nixos
zfs create -o mountpoint=legacy -o com.sun:autosnapshot=true rpool/root/home

mkfs.vfat $DISK-part2
```
