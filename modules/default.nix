{
  imports = [
    ./nvidia.nix
    ./autologin.nix
    ./user-xsession.nix
    ./noaccel-input.nix
    ./nix-flakes.nix
    ./luks-yubikey.nix
    ./ddcutil.nix
  ];
}
