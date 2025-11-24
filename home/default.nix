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

  targets.darwin = lib.optionalAttrs hostPlatform.isDarwin {
    currentHostDefaults = {
      "com.microsoft.VSCode" = {
        ApplePressAndHoldEnabled = false;
      };
    };
  };

  fonts.fontconfig.enable = !isHeadless;
  qt.enable = !isHeadless;

  gtk = {
    enable = !isHeadless;
  } // lib.optionalAttrs hostPlatform.isLinux {
    iconTheme = {
      name = "Rose Pine";
      package = pkgs.rose-pine-icon-theme;
    };
  };
}
