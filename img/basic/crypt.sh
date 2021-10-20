decrypt() {
    set -e
    echo -n "pass: "
    read -s k_user
    salt="$(head -n1 "$1"/crypt-storage/default)"
    challenge="$(echo -n $salt | openssl dgst -binary -sha512 | rbtohex)"
    response="$(ykchalresp -2 -x $challenge 2>/dev/null)"
    k_luks="$(echo -n $k_user | pbkdf2-sha512 64 1000000 $response | rbtohex)"
    echo -n "$k_luks" | hextorb | cryptsetup luksOpen "$2" "nixos-enc" --key-file=-
    mkdir -p /mnt/home
    mount -o subvol=root "/dev/partitions/fsroot" /mnt
    mount -o subvol=home "/dev/partitions/fsroot" /mnt/home
    set +e
}

cryptshell() {
    nix-shell https://github.com/sgillespie/nixos-yubikey-luks/archive/master.tar.gz --command "source /etc/profile"
}
