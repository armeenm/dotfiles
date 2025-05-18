{ lib, osConfig, isHeadless, ... }:

let
  inherit (osConfig.nixpkgs) hostPlatform;
in {
  imports = [
    ./home.nix
    ./programs.nix
    ./xdg.nix
  ] ++ lib.optionals (hostPlatform.isLinux) [
    ./services.nix
    ./systemd.nix
    ./wayland.nix
    ./gtk.nix
  ];

  fonts.fontconfig.enable = !isHeadless;

  stylix.targets.tofi.enable = false;
}
