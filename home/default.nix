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
    ./starship.nix
    ./systemd.nix
    ./waybar.nix
    ./wayland.nix
    ./xdg.nix
    ./zsh.nix
  ] ++ lib.optionals isStandalone [
    ./wrappers.nix
    ../modules/shared/nix.nix
    ../modules/shared/stylix.nix
  ];

  targets.genericLinux.enable = hostPlatform.isLinux && isStandalone;

  fonts.fontconfig.enable = !isHeadless;
  qt.enable = hostPlatform.isLinux && !isHeadless;

  gtk = {
    enable = hostPlatform.isLinux && !isHeadless;
    iconTheme = {
      name = "Rose Pine";
      package = pkgs.rose-pine-icon-theme;
    };
  };
}
