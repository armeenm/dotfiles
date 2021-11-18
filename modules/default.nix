{
  imports = [
    ./nvidia.nix
    ./autologin.nix
    ./user-xsession.nix
    ./noaccel-input.nix
    ./luks-yubikey.nix
    ./ddcutil.nix
  ];
}
