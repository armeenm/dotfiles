{ lib
, pkgs
, isHeadless
, isStandalone
, stateVersion
, ...
}:

let
  inherit (pkgs.stdenv) hostPlatform;
in {
  imports = [
    ./home.nix
    ./programs.nix
    ./services.nix
    ./systemd.nix
    ./wayland.nix
    ./xdg.nix
  ] ++ lib.optionals isStandalone [
    ./wrappers.nix
    ../modules/shared/nix.nix
    ../modules/shared/stylix.nix
  ];

  fonts.fontconfig.enable = !isHeadless;
  gtk.enable = !isHeadless;
  qt.enable = !isHeadless;
  targets.genericLinux.enable = hostPlatform.isLinux && isStandalone;
}
