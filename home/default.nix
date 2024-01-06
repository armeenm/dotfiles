{ lib, ... }:

{
  imports = [
    ./home.nix
    ./programs.nix
    ./services.nix
    ./systemd.nix
    ./xdg.nix
    ./gtk.nix
    ./wayland.nix
  ];

  fonts.fontconfig.enable = lib.mkForce true;
}
