{ lib, osConfig, ... }:

let
  inherit (osConfig.nixpkgs) hostPlatform;
in {
  imports = [
    ./home.nix
    ./programs.nix
  ] ++ lib.optionals (hostPlatform.isLinux) [
    ./services.nix
    ./systemd.nix
    ./wayland.nix
    ./xdg.nix
    ./gtk.nix
  ];

  fonts.fontconfig.enable = lib.mkForce true;
}
